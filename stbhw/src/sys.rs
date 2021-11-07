#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

#![allow(unused)]

mod bindings;

pub use bindings::*;

pub fn zeroed_tileset() -> stbhw_tileset {
    // SAFETY: `stbhw_tileset` contains no references; only raw pointers.
    unsafe { core::mem::zeroed() }
}