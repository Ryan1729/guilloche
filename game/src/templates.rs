#[derive(Debug)]
pub struct Templates {
    // We expect to have multiple templates later. We can give this a better name
    // when we know better the distinction between different templates. If we end
    // up with only one template though, that won't be the end of the world.
    t1: stbhw::Template,
}

const T1_SIZE: &[u8] = include_bytes!("t1_size.bin");
const T1_PIXELS: &[u8] = include_bytes!("t1_pixels.bin");

impl Default for Templates {
    fn default() -> Self {
        // If these unwrap's panic, then the game is not going to work anyway.
        // TODO Add compile-time asserts?
        let t1 = stbhw::Template::new(
            size_from_bytes(T1_SIZE).unwrap(),
            copy_bytes(T1_PIXELS),
        ).unwrap();

        Self {
            t1,
        }
    }
}

fn size_from_bytes(size_bytes: &[u8]) -> Result<stbhw::ImageSize, ()> {
    const BYTE_COUNT: u32 = stbhw::Int::BITS >> 3;
    if size_bytes.len() != (BYTE_COUNT * 2) as usize {
        Err(())
    } else {
        let w = stbhw::Int::from_le_bytes([
            size_bytes[0],
            size_bytes[1],
            size_bytes[2],
            size_bytes[3],
        ]);
        let h = stbhw::Int::from_le_bytes([
            size_bytes[4],
            size_bytes[5],
            size_bytes[6],
            size_bytes[7],
        ]);

        Ok(stbhw::ImageSize {
            w,
            h,
        })
    }
}

fn copy_bytes(bytes: &[u8]) -> Vec<u8> {
    let mut output = Vec::with_capacity(bytes.len());
    
    output.extend_from_slice(bytes);

    output
}