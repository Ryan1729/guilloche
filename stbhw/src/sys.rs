#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(unused)]

mod bindings;

pub use bindings::*;

use std::ffi::CStr;

fn zeroed_tileset() -> stbhw_tileset {
    // SAFETY: `stbhw_tileset` contains no references; only raw pointers.
    unsafe { core::mem::zeroed() }
}

#[test]
fn raw_sanity() {
    use core::{ptr, mem};

    const PIXEL_STRIDE: usize = 3;
    let pixel_stride: ::std::os::raw::c_int = PIXEL_STRIDE as _;

    let mut config = stbhw_config {
        is_corner: 0,
        short_side_len: 3,
        num_color: [1,1,1,1,1,1],
        num_vary_x: 3,
        num_vary_y: 3,
        corner_type_color_template: [[0; 4]; 4],
    };

    let mut template_w = -2;
    let mut template_h = -2;

    unsafe {
        stbhw_get_template_size(
            &mut config,
            &mut template_w,
            &mut template_h,
        );
    }

    // The size we got before, when running c version from the tests folder.
    assert_eq!(template_w, 27);
    assert_eq!(template_h, 49);

    let mut template_pixels = [0; 27 * 49 * PIXEL_STRIDE];

    let was_success = unsafe {
        stbhw_make_template(
            &mut config,
            template_pixels.as_mut_ptr(),
            template_w,
            template_h,
            template_w * pixel_stride,
        )
    };

    assert_eq!(was_success, 1);

    let mut ts = zeroed_tileset();

    let was_success = unsafe {
        stbhw_build_tileset_from_image(
            &mut ts,
            template_pixels.as_mut_ptr(),
            template_w * pixel_stride,
            template_w,
            template_h,
        )
    };

    assert_eq!(was_success, 1);

    let map_w = 16;
    let map_h = 16;

    let mut map_pixels = [0; 16 * 16 * PIXEL_STRIDE];

    let was_success = unsafe {
        stbhw_generate_image(
            &mut ts,
            // The comments in the .h file currently say this should always be NULL.
            ptr::null_mut(),
            map_pixels.as_mut_ptr(),
            map_w * pixel_stride,
            map_w,
            map_h,
        )
    };

    assert_eq!(was_success, 1);

    // Because we didn't change the template, the whole map image should be the
    // default #ffffff.
    for i in 0..(map_w * map_h) {
        assert_eq!(map_pixels[i as usize], 0xff, "`map_pixels[{}]` was not 0xff! {:?}", i, map_pixels);
    }

    unsafe { stbhw_free_tileset(&mut ts) } ;

    // If we got here, stbhw_free_tileset didn't panic.
    assert!(true);
}

#[test]
fn raw_error() {
    use core::{ptr, mem};

    const PIXEL_STRIDE: usize = 3;
    let pixel_stride: ::std::os::raw::c_int = PIXEL_STRIDE as _;

    let mut config = stbhw_config {
        is_corner: 0,
        short_side_len: 3,
        num_color: [1,1,1,1,1,1],
        num_vary_x: 3,
        num_vary_y: 3,
        corner_type_color_template: [[0; 4]; 4],
    };

    let mut template_w = -2;
    let mut template_h = -2;

    unsafe {
        stbhw_get_template_size(
            &mut config,
            &mut template_w,
            &mut template_h,
        );
    }

    // The size we got before, when running c version from the tests folder.
    assert_eq!(template_w, 27);
    assert_eq!(template_h, 49);

    let mut template_pixels = [0; 27 * 49 * PIXEL_STRIDE];

    let was_success = unsafe {
        stbhw_make_template(
            &mut config,
            template_pixels.as_mut_ptr(),
            template_w,
            template_h,
            template_w * pixel_stride,
        )
    };

    assert_eq!(was_success, 1);

    let mut ts = zeroed_tileset();

    let was_success = unsafe { 
        stbhw_build_tileset_from_image(
            &mut ts,
            template_pixels.as_mut_ptr(),
            template_w * pixel_stride,
            template_w,
            // We intentionally pass a zero h value to trigger an error case with an
            // error message. A zero w value causes a read to data[-1]!
            0,
        )
    };

    assert_eq!(was_success, 0);

    let last_error = unsafe { stbhw_get_last_error() };

    assert!(!last_error.is_null());

    // SAFETY: 
    // 1. We asserted above that the ptr was not null.
    // 2. `stbhw` only returns either pointers to static null-terminated
    //    strings, or null pointers from `stbhw_get_last_error`.
    let last_error = unsafe { CStr::from_ptr(last_error) }.to_str().unwrap();

    assert_eq!(last_error, "image too small for configuration");
}

#[test]
fn xs_global_default_seed() {
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 195911576);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 195911405);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 195911576);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 1788228150);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 195490147);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 1788714188);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 195911576);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 3497122649);
}

#[test]
fn xs_global_set_seed() {
    let mut seed: [u8; 16] = [
        0x12, 0x34, 0x56, 0x78,
        0x90, 0xAB, 0xCD, 0xEF,
        0x12, 0x34, 0x56, 0x78,
        0x90, 0xAB, 0xCD, 0xEF,
    ];
dbg!();
    unsafe { xs_seed_global(seed.as_mut_ptr()) }
dbg!();
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 4198859171);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 863685725);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2976477321);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2018915346);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2932402722);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 1261814658);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2971803477);
    assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2015189959);
}