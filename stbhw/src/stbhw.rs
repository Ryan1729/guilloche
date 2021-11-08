mod sys;

pub type Int = ::std::os::raw::c_int;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct ImageSize {
    pub w: Int,
    pub h: Int,
}

impl ImageSize {
    fn alloc_pixels(&self) -> Vec<u8> {
        vec![0; self.pixels_len()]
    }

    pub const fn pixels_len(&self) -> usize {
        BYTES_PER_PIXEL as usize * self.w as usize * self.h as usize
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Config {
    pub short_side_len: Int,
    pub num_vary_x: Int,
    pub num_vary_y: Int,
    pub variant: VariantConfig,
}

impl Config {
    pub fn get_template_size(&self) -> ImageSize {
        let mut cfg = self.render_config();

        get_template_size_from_cfg(&mut cfg)
    }

    pub fn make_template(&self) -> Result<Template, String> {
        let mut cfg = self.render_config();

        let size = get_template_size_from_cfg(&mut cfg);

        let mut pixels = size.alloc_pixels();

        // SAFETY: The type safety provided by `Config`, and the checks in the C
        //   code should keep those promlematic values that are likely to occur
        //   from causing too much trouble. Aborts due to asserts being hit are
        //   not currently considered too much trouble.
        let was_success = unsafe {
            sys::stbhw_make_template(
                &mut cfg,
                pixels.as_mut_ptr(),
                size.w,
                size.h,
                size.w * BYTES_PER_PIXEL,
            )
        };

        if was_success == 1 {
            Ok(Template {
                size,
                pixels,
            })
        } else {
            last_error()
        }
    }
}

fn get_template_size_from_cfg(cfg: &mut sys::stbhw_config) -> ImageSize {
    let mut w = -1;
    let mut h = -1;

    // SAFETY: The type safety provided by `Config`, and the checks in the C
    //   code should keep those promlematic values that are likely to occur
    //   from causing too much trouble. Aborts due to asserts being hit are
    //   not currently considered too much trouble.
    unsafe {
        sys::stbhw_get_template_size(
            cfg,
            &mut w,
            &mut h,
        );
    }

    ImageSize {
        w,
        h
    }
}

impl Config {
    fn render_config(&self) -> sys::stbhw_config {
        use VariantConfig::*;
        let (is_corner, num_color, corner_type_color_template) = match self.variant {
            Edge(cfg) => (
                0,
                render_edge_num_color(cfg.num_color),
                [[0; 4]; 4],
            ),
            Corner(cfg) => (
                1,
                render_corner_num_color(cfg.num_color),
                render_corner_type_color_template(cfg.corner_type_color_template),
            ),
        };

        sys::stbhw_config {
            is_corner,
            short_side_len: self.short_side_len,
            num_color,
            num_vary_x: self.num_vary_x,
            num_vary_y: self.num_vary_y,
            corner_type_color_template,
        }
    }
}

fn render_edge_num_color(num_color: [EdgeNumColour; 6]) -> [Int; 6] {
    [
        num_color[0].into(),
        num_color[1].into(),
        num_color[2].into(),
        num_color[3].into(),
        num_color[4].into(),
        num_color[5].into(),
    ]
}

fn render_corner_num_color(num_color: [CornerNumColour; 4]) -> [Int; 6] {
    [
        num_color[0].into(),
        num_color[1].into(),
        num_color[2].into(),
        num_color[3].into(),
        0,
        0,
    ]
}

fn render_corner_type_color_template(
    corner_type_color_template: [[bool; 4usize]; 4usize]
) -> [[Int; 4]; 4] {
    let mut output = [[0; 4]; 4];

    for s in 0..4 {
        for t in 0..4 {
            if corner_type_color_template[s][t] {
                output[s][t] = 1;
            }
        }
    }

    output
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VariantConfig {
    Edge(EdgeConfig),
    Corner(CornerConfig),
}

impl Default for VariantConfig {
    fn default() -> Self {
        VariantConfig::Edge(<_>::default())
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct EdgeConfig {
    pub num_color: [EdgeNumColour; 6]
}

/// The documentation embedded in the `stb_herringbone_wang.h` file says that the
/// legal values for the edge colour numbers are 1 to 8 inclusive. I tried larger
/// values, and for edge configs specifically, it seems to work anyway, at least
/// for nine. But eight edge colours produces what I would expect to be enough
/// different tiles for most if not all purposes.
// TODO Figure out real upper bound?
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum EdgeNumColour {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight
}

impl Default for EdgeNumColour {
    fn default() -> Self {
        EdgeNumColour::One
    }
}

impl From<EdgeNumColour> for Int {
    fn from(c: EdgeNumColour) -> Self {
        use EdgeNumColour::*;
        match c {
            One => 1,
            Two => 2,
            Three => 3,
            Four => 4,
            Five => 5,
            Six => 6,
            Seven => 7,
            Eight => 8,
        }
    }
}

impl TryFrom<Int> for EdgeNumColour {
    type Error = ();

    fn try_from(i: Int) -> Result<Self, Self::Error> {
        use EdgeNumColour::*;
        match i {
            1 => Ok(One),
            2 => Ok(Two),
            3 => Ok(Three),
            4 => Ok(Four),
            5 => Ok(Five),
            6 => Ok(Six),
            7 => Ok(Seven),
            8 => Ok(Eight),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct CornerConfig {
    pub num_color: [CornerNumColour; 4],
    pub corner_type_color_template: [[bool; 4usize]; 4usize],
}

/// The documentation embedded in the `stb_herringbone_wang.h` file says that the
/// legal values for the edge colour numbers are 1 to 4. I tried larger values,
/// and for corner configs specifically, generating the template produced a
/// segmentation fault.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CornerNumColour {
    One,
    Two,
    Three,
    Four,
}

impl Default for CornerNumColour {
    fn default() -> Self {
        CornerNumColour::One
    }
}

impl From<CornerNumColour> for Int {
    fn from(c: CornerNumColour) -> Self {
        use CornerNumColour::*;
        match c {
            One => 1,
            Two => 2,
            Three => 3,
            Four => 4,
        }
    }
}

impl TryFrom<Int> for CornerNumColour {
    type Error = ();

    fn try_from(i: Int) -> Result<Self, Self::Error> {
        use CornerNumColour::*;
        match i {
            1 => Ok(One),
            2 => Ok(Two),
            3 => Ok(Three),
            4 => Ok(Four),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Template {
    size: ImageSize,
    pixels: Vec<u8>,
}

#[derive(Debug)]
pub struct SizeMismatchError;

impl core::fmt::Display for SizeMismatchError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Byte count implied by ImageSize did not match pixels length!")
    }
}

impl std::error::Error for SizeMismatchError {}

impl Template {
    pub fn size(&self) -> &ImageSize {
        &self.size
    }

    pub fn pixels(&self) -> &[u8] {
        &self.pixels
    }

    pub fn new(size: ImageSize, pixels: Vec<u8>) -> Result<Self, SizeMismatchError> {
        if size.pixels_len() != pixels.len() {
            Err(SizeMismatchError)
        } else {
            Ok(Self {
                size,
                pixels,
            })
        }
    }
}

pub const BYTES_PER_PIXEL: Int = 3;

fn last_error<T>() -> Result<T, String> {
    // SAFETY: `stbhw_get_last_error` is technically not thread safe, in that
    // if multiple threads attempt to access the error data races can occur.
    // But, since the error strings are static, the worst that can happen
    // is we return the wrong error message.
    // TODO: Synchronize calls to `stbhw_get_last_error`?
    let error_or_null = unsafe { sys::stbhw_get_last_error() };

    let s = if error_or_null.is_null() {
        "Error string was null!".to_string()
    } else {
        // SAFETY:
        // 1. We checked above that the ptr was not null.
        // 2. `stbhw` only returns either pointers to static null-terminated
        //    strings, or null pointers from `stbhw_get_last_error`.
        let last_error = unsafe { std::ffi::CStr::from_ptr(error_or_null) }
            .to_str()
            .map_err(|e| e.to_string())?;

        last_error.to_string()
    };

    Err(s)
}

#[derive(Debug)]
pub struct Tileset {
    tileset: sys::stbhw_tileset,
}

impl Drop for Tileset {
    fn drop(&mut self) {
        // SAFTEY: Rust should guarentee that `drop` is called at most once.
        unsafe {
            sys::stbhw_free_tileset(&mut self.tileset);
        }
    }
}

impl Tileset {
    pub fn from_template(template: &mut Template) -> Result<Self, String> {
        let mut tileset = sys::zeroed_tileset();

        // SAFETY: Because we only expose read only getters for `Template`'s
        //   fields, we know that the length of `pixels` and the length described
        //   by `size` match up.
        let was_success = unsafe {
            sys::stbhw_build_tileset_from_image(
                &mut tileset,
                template.pixels.as_mut_ptr(),
                template.size.w * BYTES_PER_PIXEL,
                template.size.w,
                template.size.h,
            )
        };

        if was_success == 1 {
            Ok(Self {
                tileset
            })
        } else {
            last_error()
        }
    }

    pub fn generate_image(&mut self, map_size: ImageSize) -> Result<Map, String> {
        let mut pixels = map_size.alloc_pixels();

        // SAFETY: Because we alloc the pixels in this method, we know that the
        //   length of `pixels` and the length described by `map_size` match up.
        let was_success = unsafe {
            sys::stbhw_generate_image(
                &mut self.tileset,
                // The comments in the .h file currently say this should always
                // be NULL.
                core::ptr::null_mut(),
                pixels.as_mut_ptr(),
                map_size.w * BYTES_PER_PIXEL,
                map_size.w,
                map_size.h,
            )
        };

        if was_success == 1 {
            Ok(Map {
                size: map_size,
                pixels,
            })
        } else {
            last_error()
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Map {
    pub size: ImageSize,
    pub pixels: Vec<u8>,
}

pub type Seed = [u8; 16];

pub fn xs_seed_global(mut seed: Seed) {
    // SAFETY: Any byte value is valid as a seed. 0 defaults to a different fixed
    // seed, so even that is acceptable.
    unsafe { sys::xs_seed_global(seed.as_mut_ptr()) }
}

#[cfg(test)]
mod tests {
    use super::{*, sys::*};

    #[test]
    fn raw_error() {
        use std::ffi::CStr;

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
    fn wrapped_error() {
        use EdgeNumColour::One;

        let config = Config {
            short_side_len: 3,
            num_vary_x: 3,
            num_vary_y: 3,
            variant: VariantConfig::Edge(EdgeConfig {
                num_color: [One,One,One,One,One,One],
            }),
        };

        let ImageSize {
            w: template_w,
            h: template_h
        } = config.get_template_size();

        // The size we got before, when running c version from the tests folder.
        assert_eq!(template_w, 27);
        assert_eq!(template_h, 49);

        let mut template = config.make_template().unwrap();

        // We intentionally pass a zero h value to trigger an error case with an
        // error message. A zero w value causes a read to data[-1]!
        // It's worth noting this particular error case cannot be triggered without
        // mutating a private field. Are there still any errors that can be triggered
        // without doing that?

        template.size.h = 0;

        let error = Tileset::from_template(&mut template).unwrap_err();

        assert_eq!(error, "image too small for configuration");
    }

    #[cfg_attr(not(feature = "unsynced-tests"), test)]
    #[cfg_attr(feature = "unsynced-tests", allow(unused))]
    fn rng_touching_tests() {
        // First so the default is set.
        xs_global_default_seed();

        raw_sanity();
        wrapped_sanity();

        xs_global_set_seed();
    }

    #[cfg_attr(feature = "unsynced-tests", test)]
    fn raw_sanity() {
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
                core::ptr::null_mut(),
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

    #[cfg_attr(feature = "unsynced-tests", test)]
    fn wrapped_sanity() {
        use EdgeNumColour::One;

        let config = Config {
            short_side_len: 3,
            num_vary_x: 3,
            num_vary_y: 3,
            variant: VariantConfig::Edge(EdgeConfig {
                num_color: [One,One,One,One,One,One],
            }),
        };

        let ImageSize {
            w: template_w,
            h: template_h
        } = config.get_template_size();

        // The size we got before, when running c version from the tests folder.
        assert_eq!(template_w, 27);
        assert_eq!(template_h, 49);

        let mut template = config.make_template().unwrap();

        let mut ts = Tileset::from_template(&mut template).unwrap();

        let map = ts.generate_image(ImageSize {
            w: 16,
            h: 16
        }).unwrap();

        // Because we didn't change the template, the whole map image should be the
        // default #ffffff.
        for i in 0..(map.size.w * map.size.h) {
            assert_eq!(map.pixels[i as usize], 0xff, "`map.pixels[{}]` was not 0xff! {:?}", i, map.pixels);
        }

        drop(ts);

        // If we got here, stbhw_free_tileset didn't panic.
        assert!(true);
    }

    #[cfg_attr(feature = "unsynced-tests", test)]
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

    #[cfg_attr(feature = "unsynced-tests", test)]
    fn xs_global_set_seed() {
        let mut seed: [u8; 16] = [
            0x12, 0x34, 0x56, 0x78,
            0x90, 0xAB, 0xCD, 0xEF,
            0x12, 0x34, 0x56, 0x78,
            0x90, 0xAB, 0xCD, 0xEF,
        ];

        unsafe { sys::xs_seed_global(seed.as_mut_ptr()) }

        assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 4198859171);
        assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 863685725);
        assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2976477321);
        assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2018915346);
        assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2932402722);
        assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 1261814658);
        assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2971803477);
        assert_eq!(unsafe { xs_global_u32(0, 0xFFFF_FFFF) }, 2015189959);
    }
}
