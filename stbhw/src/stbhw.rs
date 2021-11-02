mod sys;

pub type Int = ::std::os::raw::c_int;

pub struct Config {
    pub short_side_len: Int,
    pub num_vary_x: Int,
    pub num_vary_y: Int,
    pub variant: VariantConfig,
}

pub enum VariantConfig {
    Edge(EdgeConfig),
    Corner(CornerConfig),
}

pub struct EdgeConfig {
    pub num_color: [EdgeNumColour; 4]
}

/// The documentation embedded in the `stb_herringbone_wang.h` file says that the
/// legal values for the edge colour numbers are 1 to 8 inclusive. I tried larger
/// values, and for edge configs specifically, it seems to work anyway, at least
/// for nine. But eight edge colours produces what I would expect to be enough
/// different tiles for most if not all purposes.
// TODO Figure out real upper bound?
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

pub struct CornerConfig {
    pub num_color: [CornerNumColour; 6],
    pub corner_type_color_template: [[bool; 4usize]; 4usize],
}

/// The documentation embedded in the `stb_herringbone_wang.h` file says that the
/// legal values for the edge colour numbers are 1 to 4. I tried larger values,
/// and for corner configs specifically, generating the template produced a
/// segmentation fault.
pub enum CornerNumColour {
    One,
    Two,
    Three,
    Four,
}
