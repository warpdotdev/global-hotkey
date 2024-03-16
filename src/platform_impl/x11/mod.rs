// Copyright 2022-2022 Tauri Programme within The Commons Conservancy
// SPDX-License-Identifier: Apache-2.0
// SPDX-License-Identifier: MIT

use std::collections::{BTreeMap, HashMap};

use crossbeam_channel::{unbounded, Receiver, Sender};
use keyboard_types::{Code, Modifiers};
use x11_dl::{
    keysym,
    xlib::{self, Xlib, _XDisplay},
};
use x11rb::{
    connection::Connection,
    errors::ReplyError,
    protocol::{
        xinput::KeyCode,
        xproto::{self, ConnectionExt, GrabMode, KeyButMask, ModMask},
        ErrorKind, Event,
    },
    x11_utils::X11Error,
    xcb_ffi::XCBConnection,
};

use crate::{hotkey::HotKey, GlobalHotKeyEvent};

enum ThreadMessage {
    RegisterHotKey(HotKey, Sender<crate::Result<()>>),
    RegisterHotKeys(Vec<HotKey>, Sender<crate::Result<()>>),
    UnRegisterHotKey(HotKey, Sender<crate::Result<()>>),
    UnRegisterHotKeys(Vec<HotKey>, Sender<crate::Result<()>>),
    DropThread,
}

pub struct GlobalHotKeyManager {
    thread_tx: Sender<ThreadMessage>,
}

impl GlobalHotKeyManager {
    pub fn new() -> crate::Result<Self> {
        let (thread_tx, thread_rx) = unbounded();
        std::thread::spawn(|| events_processor(thread_rx));
        Ok(Self { thread_tx })
    }

    pub fn register(&self, hotkey: HotKey) -> crate::Result<()> {
        let (tx, rx) = crossbeam_channel::bounded(1);
        let _ = self
            .thread_tx
            .send(ThreadMessage::RegisterHotKey(hotkey, tx));

        if let Ok(result) = rx.recv() {
            result?;
        }

        Ok(())
    }

    pub fn unregister(&self, hotkey: HotKey) -> crate::Result<()> {
        let (tx, rx) = crossbeam_channel::bounded(1);
        let _ = self
            .thread_tx
            .send(ThreadMessage::UnRegisterHotKey(hotkey, tx));

        if let Ok(result) = rx.recv() {
            result?;
        }

        Ok(())
    }

    pub fn register_all(&self, hotkeys: &[HotKey]) -> crate::Result<()> {
        let (tx, rx) = crossbeam_channel::bounded(1);
        let _ = self
            .thread_tx
            .send(ThreadMessage::RegisterHotKeys(hotkeys.to_vec(), tx));

        if let Ok(result) = rx.recv() {
            result?;
        }

        Ok(())
    }

    pub fn unregister_all(&self, hotkeys: &[HotKey]) -> crate::Result<()> {
        let (tx, rx) = crossbeam_channel::bounded(1);
        let _ = self
            .thread_tx
            .send(ThreadMessage::UnRegisterHotKeys(hotkeys.to_vec(), tx));

        if let Ok(result) = rx.recv() {
            result?;
        }

        Ok(())
    }
}

impl Drop for GlobalHotKeyManager {
    fn drop(&mut self) {
        let _ = self.thread_tx.send(ThreadMessage::DropThread);
    }
}

fn register_hotkey_xcb(
    conn: &XCBConnection,
    root_win: u32,
    hotkeys: &mut BTreeMap<u8, Vec<(u32, ModMask, bool)>>,
    hotkey: HotKey,
    keysym_keycode_map: &HashMap<u32, u8>,
) -> crate::Result<()> {
    let IGNORED_MODS_XCB: [ModMask; 4] = [
        ModMask::default(),
        ModMask::M2,
        ModMask::LOCK,
        ModMask::M2 | ModMask::LOCK,
    ];

    let (modifiers, keysym) = (
        modifiers_to_xcb_modmask(hotkey.mods),
        keycode_to_x11_scancode(hotkey.key),
    );

    let Some(key) = keysym.and_then(|k| keysym_keycode_map.get(&k)).cloned() else {
        return Err(crate::Error::FailedToRegister(format!(
            "Unable to register accelerator (unknown scancode for this key: {}).",
            hotkey.key
        )));
    };

    for m in IGNORED_MODS_XCB {
        match conn.grab_key(
            false,
            root_win,
            modifiers | m,
            key,
            GrabMode::ASYNC,
            GrabMode::ASYNC,
        ) {
            Ok(cookie) => {
                if let Err(ReplyError::X11Error(X11Error {
                    error_kind: ErrorKind::Access,
                    ..
                })) = cookie.check()
                {
                    let _ =
                        unregister_hotkey_xcb(conn, root_win, hotkeys, hotkey, keysym_keycode_map);
                    return Err(crate::Error::AlreadyRegistered(hotkey));
                }
            }
            Err(err) => {
                return Err(crate::Error::OsError(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    err,
                )))
            }
        }
    }

    let keycode = KeyCode::from(key);
    let entry = hotkeys.entry(keycode).or_default();
    match entry.iter().find(|e| e.1 == modifiers) {
        None => {
            entry.push((hotkey.id(), modifiers, false));
            Ok(())
        }
        Some(_) => Err(crate::Error::AlreadyRegistered(hotkey)),
    }
}

fn unregister_hotkey_xcb(
    conn: &XCBConnection,
    root_win: u32,
    hotkeys: &mut BTreeMap<u8, Vec<(u32, ModMask, bool)>>,
    hotkey: HotKey,
    keysym_keycode_map: &HashMap<u32, u8>,
) -> crate::Result<()> {
    let IGNORED_MODS_XCB: [ModMask; 4] = [
        ModMask::default(),
        ModMask::M2,
        ModMask::LOCK,
        ModMask::M2 | ModMask::LOCK,
    ];

    let (modifiers, keysym) = (
        modifiers_to_xcb_modmask(hotkey.mods),
        keycode_to_x11_scancode(hotkey.key),
    );

    let Some(key) = keysym.and_then(|k| keysym_keycode_map.get(&k)).cloned() else {
        return Err(crate::Error::FailedToUnRegister(hotkey));
    };

    for m in IGNORED_MODS_XCB {
        let _ = conn
            .ungrab_key(key, root_win, modifiers | m)
            .map(|cookie| cookie.ignore_error());
    }

    let keycode = KeyCode::from(key);
    let entry = hotkeys.entry(keycode).or_default();
    entry.retain(|k| k.1 != modifiers);

    Ok(())
}

fn events_processor(thread_rx: Receiver<ThreadMessage>) {
    //                           key    id,  mods, pressed
    let mut hotkeys = BTreeMap::<u8, Vec<(u32, ModMask, bool)>>::new();

    if let Err(error) = x11rb::xcb_ffi::load_libxcb() {
        #[cfg(debug_assertions)]
        eprintln!("Failed to dynamically load libxcb: {error:#}.  Make sure the library is installed on the host system.");
        return;
    }

    let Ok((conn, _)) = x11rb::xcb_ffi::XCBConnection::connect(None) else {
        #[cfg(debug_assertions)]
        eprintln!("Failed to open connection to X11 server!");
        return;
    };

    const MIN_KEYCODE: u8 = 8;
    const MAX_KEYCODE: u8 = 255;

    let mapping = match conn.get_keyboard_mapping(MIN_KEYCODE, MAX_KEYCODE - MIN_KEYCODE + 1) {
        Ok(cookie) => {
            let Ok(mapping) = cookie.reply() else {
                return;
            };
            mapping
        }
        Err(err) => {
            #[cfg(debug_assertions)]
            eprintln!("Failed to get keysym to keycode mapping!");
            return;
        }
    };
    let mut keysym_keycode_map = HashMap::<u32, u8>::default();
    for (keycode, keysyms) in mapping
        .keysyms
        .chunks(mapping.keysyms_per_keycode as usize)
        .enumerate()
    {
        for keysym in keysyms {
            if *keysym != x11rb::NO_SYMBOL {
                keysym_keycode_map.insert(*keysym, keycode as u8 + MIN_KEYCODE);
            }
        }
    }

    let setup = conn.setup();
    let Some(root_win) = setup.roots.first().map(|screen| screen.root) else {
        #[cfg(debug_assertions)]
        eprintln!("Could not find a root window!");
        return;
    };

    loop {
        match conn.poll_for_event() {
            Ok(Some(event)) => handle_event(event, &mut hotkeys),
            Ok(None) => {}
            Err(error) => {
                #[cfg(debug_assertions)]
                eprintln!("Encountered connection error while polling for events: {error:#}");
                break;
            }
        }

        if let Ok(msg) = thread_rx.try_recv() {
            match msg {
                ThreadMessage::RegisterHotKey(hotkey, tx) => {
                    let _ = tx.send(register_hotkey_xcb(
                        &conn,
                        root_win,
                        &mut hotkeys,
                        hotkey,
                        &keysym_keycode_map,
                    ));
                }
                ThreadMessage::RegisterHotKeys(keys, tx) => {
                    for hotkey in keys {
                        if let Err(e) = register_hotkey_xcb(
                            &conn,
                            root_win,
                            &mut hotkeys,
                            hotkey,
                            &keysym_keycode_map,
                        ) {
                            let _ = tx.send(Err(e));
                        }
                        let _ = tx.send(Ok(()));
                    }
                }
                ThreadMessage::UnRegisterHotKey(hotkey, tx) => {
                    let _ = tx.send(unregister_hotkey_xcb(
                        &conn,
                        root_win,
                        &mut hotkeys,
                        hotkey,
                        &keysym_keycode_map,
                    ));
                }
                ThreadMessage::UnRegisterHotKeys(keys, tx) => {
                    for hotkey in keys {
                        if let Err(e) = unregister_hotkey_xcb(
                            &conn,
                            root_win,
                            &mut hotkeys,
                            hotkey,
                            &keysym_keycode_map,
                        ) {
                            let _ = tx.send(Err(e));
                        }
                        let _ = tx.send(Ok(()));
                    }
                }
                ThreadMessage::DropThread => {
                    break;
                }
            }
        }
    }
}

fn handle_event(event: Event, hotkeys: &mut BTreeMap<u8, Vec<(u32, ModMask, bool)>>) {
    match event {
        Event::KeyPress(xproto::KeyPressEvent {
            detail: keycode,
            state,
            response_type,
            ..
        })
        | Event::KeyRelease(xproto::KeyReleaseEvent {
            detail: keycode,
            state,
            response_type,
            ..
        }) => {
            let event_mods = keybutmask_to_modmask(state)
                & (ModMask::CONTROL | ModMask::SHIFT | ModMask::M1 | ModMask::M4);

            if let Some(entry) = hotkeys.get_mut(&keycode) {
                match response_type {
                    xproto::KEY_PRESS_EVENT => {
                        for (id, mods, pressed) in entry {
                            if event_mods == *mods && !*pressed {
                                GlobalHotKeyEvent::send(GlobalHotKeyEvent {
                                    id: *id,
                                    state: crate::HotKeyState::Pressed,
                                });
                                *pressed = true;
                            }
                        }
                    }
                    xproto::KEY_RELEASE_EVENT => {
                        for (id, _, pressed) in entry {
                            if *pressed {
                                GlobalHotKeyEvent::send(GlobalHotKeyEvent {
                                    id: *id,
                                    state: crate::HotKeyState::Released,
                                });
                                *pressed = false;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }
}

fn keycode_to_x11_scancode(key: Code) -> Option<u32> {
    Some(match key {
        Code::KeyA => keysym::XK_A,
        Code::KeyB => keysym::XK_B,
        Code::KeyC => keysym::XK_C,
        Code::KeyD => keysym::XK_D,
        Code::KeyE => keysym::XK_E,
        Code::KeyF => keysym::XK_F,
        Code::KeyG => keysym::XK_G,
        Code::KeyH => keysym::XK_H,
        Code::KeyI => keysym::XK_I,
        Code::KeyJ => keysym::XK_J,
        Code::KeyK => keysym::XK_K,
        Code::KeyL => keysym::XK_L,
        Code::KeyM => keysym::XK_M,
        Code::KeyN => keysym::XK_N,
        Code::KeyO => keysym::XK_O,
        Code::KeyP => keysym::XK_P,
        Code::KeyQ => keysym::XK_Q,
        Code::KeyR => keysym::XK_R,
        Code::KeyS => keysym::XK_S,
        Code::KeyT => keysym::XK_T,
        Code::KeyU => keysym::XK_U,
        Code::KeyV => keysym::XK_V,
        Code::KeyW => keysym::XK_W,
        Code::KeyX => keysym::XK_X,
        Code::KeyY => keysym::XK_Y,
        Code::KeyZ => keysym::XK_Z,
        Code::Backslash => keysym::XK_backslash,
        Code::BracketLeft => keysym::XK_bracketleft,
        Code::BracketRight => keysym::XK_bracketright,
        Code::Backquote => keysym::XK_quoteleft,
        Code::Comma => keysym::XK_comma,
        Code::Digit0 => keysym::XK_0,
        Code::Digit1 => keysym::XK_1,
        Code::Digit2 => keysym::XK_2,
        Code::Digit3 => keysym::XK_3,
        Code::Digit4 => keysym::XK_4,
        Code::Digit5 => keysym::XK_5,
        Code::Digit6 => keysym::XK_6,
        Code::Digit7 => keysym::XK_7,
        Code::Digit8 => keysym::XK_8,
        Code::Digit9 => keysym::XK_9,
        Code::Equal => keysym::XK_equal,
        Code::Minus => keysym::XK_minus,
        Code::Period => keysym::XK_period,
        Code::Quote => keysym::XK_leftsinglequotemark,
        Code::Semicolon => keysym::XK_semicolon,
        Code::Slash => keysym::XK_slash,
        Code::Backspace => keysym::XK_BackSpace,
        Code::CapsLock => keysym::XK_Caps_Lock,
        Code::Enter => keysym::XK_Return,
        Code::Space => keysym::XK_space,
        Code::Tab => keysym::XK_Tab,
        Code::Delete => keysym::XK_Delete,
        Code::End => keysym::XK_End,
        Code::Home => keysym::XK_Home,
        Code::Insert => keysym::XK_Insert,
        Code::PageDown => keysym::XK_Page_Down,
        Code::PageUp => keysym::XK_Page_Up,
        Code::ArrowDown => keysym::XK_Down,
        Code::ArrowLeft => keysym::XK_Left,
        Code::ArrowRight => keysym::XK_Right,
        Code::ArrowUp => keysym::XK_Up,
        Code::Numpad0 => keysym::XK_KP_0,
        Code::Numpad1 => keysym::XK_KP_1,
        Code::Numpad2 => keysym::XK_KP_2,
        Code::Numpad3 => keysym::XK_KP_3,
        Code::Numpad4 => keysym::XK_KP_4,
        Code::Numpad5 => keysym::XK_KP_5,
        Code::Numpad6 => keysym::XK_KP_6,
        Code::Numpad7 => keysym::XK_KP_7,
        Code::Numpad8 => keysym::XK_KP_8,
        Code::Numpad9 => keysym::XK_KP_9,
        Code::NumpadAdd => keysym::XK_KP_Add,
        Code::NumpadDecimal => keysym::XK_KP_Decimal,
        Code::NumpadDivide => keysym::XK_KP_Divide,
        Code::NumpadMultiply => keysym::XK_KP_Multiply,
        Code::NumpadSubtract => keysym::XK_KP_Subtract,
        Code::Escape => keysym::XK_Escape,
        Code::PrintScreen => keysym::XK_Print,
        Code::ScrollLock => keysym::XK_Scroll_Lock,
        Code::AudioVolumeDown => keysym::XF86XK_AudioLowerVolume,
        Code::AudioVolumeMute => keysym::XF86XK_AudioMute,
        Code::AudioVolumeUp => keysym::XF86XK_AudioRaiseVolume,
        Code::NumLock => keysym::XK_F1,
        Code::F1 => keysym::XK_F1,
        Code::F2 => keysym::XK_F2,
        Code::F3 => keysym::XK_F3,
        Code::F4 => keysym::XK_F4,
        Code::F5 => keysym::XK_F5,
        Code::F6 => keysym::XK_F6,
        Code::F7 => keysym::XK_F7,
        Code::F8 => keysym::XK_F8,
        Code::F9 => keysym::XK_F9,
        Code::F10 => keysym::XK_F10,
        Code::F11 => keysym::XK_F11,
        Code::F12 => keysym::XK_F12,

        _ => return None,
    })
}

fn modifiers_to_xcb_modmask(modifiers: Modifiers) -> ModMask {
    let mut mask = ModMask::default();
    if modifiers.contains(Modifiers::SHIFT) {
        mask |= ModMask::SHIFT;
    }
    if modifiers.intersects(Modifiers::SUPER | Modifiers::META) {
        mask |= ModMask::M4;
    }
    if modifiers.contains(Modifiers::ALT) {
        mask |= ModMask::M1;
    }
    if modifiers.contains(Modifiers::CONTROL) {
        mask |= ModMask::CONTROL;
    }
    mask
}

fn keybutmask_to_modmask(mask: KeyButMask) -> ModMask {
    mask.bits().into()
}
