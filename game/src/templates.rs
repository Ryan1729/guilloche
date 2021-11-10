#[derive(Debug)]
pub struct Templates {
    // We expect to have multiple templates later. We can give this a better name
    // when we know better the distinction between different templates. If we end
    // up with only one template though, that won't be the end of the world.
    pub t1: stbhw::Template,
}

const T1_SIZE_BYTES: &[u8] = include_bytes!("t1_size.bin");
const T1_SIZE: stbhw::ImageSize = size_from_bytes_or_minus_one(T1_SIZE_BYTES);
const T1_PIXELS: &[u8] = include_bytes!("t1_pixels.bin");

impl Default for Templates {
    fn default() -> Self {
        compile_time_assert!{
            T1_SIZE.w > 0 && T1_SIZE.h > 0
        }
        compile_time_assert!{
            stbhw::Template::pixel_len_matches(T1_SIZE, T1_PIXELS)
        }

        // If this unwrap panics, then the game is not going to work anyway.
        // The `compile_time_assert`s above should prevent a panic to the 
        // extent possible.
        let t1 = stbhw::Template::new(
            T1_SIZE,
            copy_bytes(T1_PIXELS),
        ).unwrap();

        Self {
            t1,
        }
    }
}

const fn size_from_bytes(size_bytes: &[u8]) -> Result<stbhw::ImageSize, ()> {
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

const fn size_from_bytes_or_minus_one(size_bytes: &[u8]) -> stbhw::ImageSize {
    match size_from_bytes(size_bytes) {
        Ok(size) => size,
        Err(_) => stbhw::ImageSize{w: -1, h: -1},
    }
}

fn copy_bytes(bytes: &[u8]) -> Vec<u8> {
    let mut output = Vec::with_capacity(bytes.len());
    
    output.extend_from_slice(bytes);

    output
}