// Copyright 2022-2022 Tauri Programme within The Commons Conservancy
// SPDX-License-Identifier: Apache-2.0
// SPDX-License-Identifier: MIT

use std::collections::{BTreeMap, HashMap};

use crossbeam_channel::{unbounded, Receiver, Sender};
use itertools::Itertools;
use keyboard_types::{Code, Modifiers};
use x11_dl::keysym;
use x11rb::{
    connection::Connection,
    protocol::{
        xinput::KeyCode,
        xkb::ConnectionExt as _,
        xproto::{self, ConnectionExt as _, GrabMode, ModMask},
        Event,
    },
    reexports::x11rb_protocol::protocol::xkb,
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

type HotKeyStateMap = BTreeMap<KeyCode, Vec<HotKeyState>>;

struct HotKeyState {
    id: u32,
    modifiers: ModMask,
    pressed: bool,
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

fn register_hotkey(
    conn: &XCBConnection,
    root_win: u32,
    hotkeys: &mut HotKeyStateMap,
    hotkey: HotKey,
    keysym_keycode_map: &HashMap<u32, KeyCode>,
) -> crate::Result<()> {
    let modifiers = hotkey.to_xcb_modifiers();
    let Some(keycode) = hotkey.to_x11_keycode(keysym_keycode_map) else {
        return Err(crate::Error::FailedToUnRegister(hotkey));
    };

    // Attempt to grab all key + modifier combinations, consuming any errors
    // from requests that produced a response.
    let results = modifiers
        .into_iter()
        .map(|m| {
            conn.grab_key(
                false,
                root_win,
                m,
                keycode,
                GrabMode::ASYNC,
                GrabMode::ASYNC,
            )
        })
        .map_ok(|cookie| cookie.check())
        .collect_vec();

    // Return an error if there were any connection errors.
    let results: Vec<_> = match results.into_iter().collect() {
        Ok(results) => results,
        Err(err) => {
            return Err(crate::Error::OsError(std::io::Error::new(
                std::io::ErrorKind::Other,
                err,
            )));
        }
    };

    // If we failed to grab any of the key+modifier combinations, ungrab all
    // of them and return an error.
    if results.into_iter().any(|result| result.is_err()) {
        let _ = unregister_hotkey(conn, root_win, hotkeys, hotkey, keysym_keycode_map);
        return Err(crate::Error::AlreadyRegistered(hotkey));
    }

    let entry = hotkeys.entry(keycode).or_default();
    let modmask = hotkey.to_xcb_modmask();
    match entry.iter().find(|e| e.modifiers == modmask) {
        None => {
            entry.push(HotKeyState {
                id: hotkey.id(),
                modifiers: modmask,
                pressed: false,
            });
            Ok(())
        }
        Some(_) => Err(crate::Error::AlreadyRegistered(hotkey)),
    }
}

fn unregister_hotkey(
    conn: &XCBConnection,
    root_win: u32,
    hotkeys: &mut HotKeyStateMap,
    hotkey: HotKey,
    keysym_keycode_map: &HashMap<u32, KeyCode>,
) -> crate::Result<()> {
    let modifiers = hotkey.to_xcb_modifiers();
    let Some(keycode) = hotkey.to_x11_keycode(keysym_keycode_map) else {
        return Err(crate::Error::FailedToUnRegister(hotkey));
    };

    // Issue all UngrabKey requests before waiting for responses.
    let cookies = modifiers
        .into_iter()
        .map(|m| conn.ungrab_key(keycode, root_win, m))
        .collect_vec();

    // Consume errors for all requests that produced a response.
    cookies
        .into_iter()
        .filter_map(|result| result.ok())
        .for_each(|cookie| cookie.ignore_error());

    let entry = hotkeys.entry(keycode as KeyCode).or_default();
    let modmask = hotkey.to_xcb_modmask();
    entry.retain(|k| k.modifiers != modmask);

    Ok(())
}

fn events_processor(thread_rx: Receiver<ThreadMessage>) {
    let mut hotkeys = HotKeyStateMap::default();

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

    // Enable use of xkb extensions (needed to enable detectable key repeat).
    match conn.xkb_use_extension(1, 0) {
        Ok(cookie) => cookie.discard_reply_and_errors(),
        Err(error) => {
            #[cfg(debug_assertions)]
            eprintln!("Error while enabling xkb: {error:#}");
            return;
        }
    }

    // Enable detectable key repeat.
    match conn.xkb_per_client_flags(
        xkb::ID::USE_CORE_KBD.into(),
        xkb::PerClientFlag::DETECTABLE_AUTO_REPEAT,
        xkb::PerClientFlag::DETECTABLE_AUTO_REPEAT,
        xkb::BoolCtrl::default(),
        xkb::BoolCtrl::default(),
        xkb::BoolCtrl::default(),
    ) {
        Ok(cookie) => cookie.discard_reply_and_errors(),
        Err(error) => {
            #[cfg(debug_assertions)]
            eprintln!("Error while enabling detectable key repeat: {error:#}");
            return;
        }
    }

    let Some(keysym_keycode_map) = get_keysym_keycode_map(&conn) else {
        return;
    };

    let Some(root_win) = conn.setup().roots.first().map(|screen| screen.root) else {
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
                    let _ = tx.send(register_hotkey(
                        &conn,
                        root_win,
                        &mut hotkeys,
                        hotkey,
                        &keysym_keycode_map,
                    ));
                }
                ThreadMessage::RegisterHotKeys(keys, tx) => {
                    // Try registering all provided hotkeys, returning the first error encountered
                    // (if any).
                    let _ = tx.send(
                        keys.into_iter()
                            .try_for_each(|hotkey| {
                                register_hotkey(
                                    &conn,
                                    root_win,
                                    &mut hotkeys,
                                    hotkey,
                                    &keysym_keycode_map,
                                )
                            })
                            .map(|_| ()),
                    );
                }
                ThreadMessage::UnRegisterHotKey(hotkey, tx) => {
                    let _ = tx.send(unregister_hotkey(
                        &conn,
                        root_win,
                        &mut hotkeys,
                        hotkey,
                        &keysym_keycode_map,
                    ));
                }
                ThreadMessage::UnRegisterHotKeys(keys, tx) => {
                    // Try unregistering all provided hotkeys, returning the first error encountered
                    // (if any).
                    let _ = tx.send(
                        keys.into_iter()
                            .try_for_each(|hotkey| {
                                unregister_hotkey(
                                    &conn,
                                    root_win,
                                    &mut hotkeys,
                                    hotkey,
                                    &keysym_keycode_map,
                                )
                            })
                            .map(|_| ()),
                    );
                }
                ThreadMessage::DropThread => {
                    break;
                }
            }
        }
    }
}

fn handle_event(event: Event, hotkeys: &mut HotKeyStateMap) {
    match event {
        Event::KeyPress(xproto::KeyPressEvent {
            detail: keycode,
            state,
            ..
        }) => {
            let event_mods = ModMask::from(state.bits())
                & (ModMask::CONTROL | ModMask::SHIFT | ModMask::M1 | ModMask::M4);
            let Some(entry) = hotkeys.get_mut(&keycode) else {
                return;
            };
            for HotKeyState {
                id,
                modifiers,
                pressed,
            } in entry
            {
                if event_mods == *modifiers && !*pressed {
                    GlobalHotKeyEvent::send(GlobalHotKeyEvent {
                        id: *id,
                        state: crate::HotKeyState::Pressed,
                    });
                    *pressed = true;
                }
            }
        }
        Event::KeyRelease(xproto::KeyReleaseEvent {
            detail: keycode, ..
        }) => {
            let Some(entry) = hotkeys.get_mut(&keycode) else {
                return;
            };
            for HotKeyState { id, pressed, .. } in entry {
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

fn get_keysym_keycode_map(conn: &XCBConnection) -> Option<HashMap<u32, KeyCode>> {
    let setup = conn.setup();
    let mapping = match conn
        .get_keyboard_mapping(setup.min_keycode, setup.max_keycode - setup.min_keycode + 1)
        .map(|cookie| cookie.reply())
    {
        Ok(Ok(mapping)) => mapping,
        Ok(Err(err)) => {
            #[cfg(debug_assertions)]
            eprintln!("Failed to get keysym to keycode mapping: {err:#}");
            return None;
        }
        Err(err) => {
            #[cfg(debug_assertions)]
            eprintln!("Failed to get keysym to keycode mapping: {err:#}");
            return None;
        }
    };

    let mut keysym_keycode_map = HashMap::<u32, KeyCode>::default();
    for (keycode, keysyms) in mapping
        .keysyms
        .chunks(mapping.keysyms_per_keycode as usize)
        .enumerate()
    {
        // Add min_keycode to the chunk index to get the actual keycode.
        let keycode = keycode as u8 + setup.min_keycode;

        for keysym in keysyms {
            if *keysym != x11rb::NO_SYMBOL {
                keysym_keycode_map.insert(*keysym, keycode);
            }
        }
    }
    Some(keysym_keycode_map)
}

impl HotKey {
    fn to_xcb_modifiers(&self) -> Vec<ModMask> {
        let modifiers = self.to_xcb_modmask();
        [
            ModMask::default(),
            ModMask::M2,
            ModMask::LOCK,
            ModMask::M2 | ModMask::LOCK,
        ]
        .into_iter()
        .map(|m| m | modifiers)
        .collect_vec()
    }

    fn to_xcb_modmask(&self) -> ModMask {
        let modifiers = &self.mods;
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

    fn to_x11_keycode(&self, keysym_keycode_map: &HashMap<u32, KeyCode>) -> Option<KeyCode> {
        let keysym = keycode_to_x11_scancode(self.key);
        keysym.and_then(|k| keysym_keycode_map.get(&k)).cloned()
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
