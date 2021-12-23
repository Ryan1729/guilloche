#[derive(Debug)]
pub struct Templates {
    // We expect to have multiple templates later. We can give this a better name
    // when we know better the distinction between different templates. If we end
    // up with only one template though, that won't be the end of the world.
    pub t1: stbhw::Template,
    // Ditto for decks.
    pub d1: Deck1,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TileKind {
    Floor,
    Wall,
    Npc,
}

impl Default for TileKind {
    fn default() -> Self {
        TileKind::Floor
    }
}

xs::counted_enum_def!{
    EdgeType {
        UpperLeft,
        Upper,
        UpperRight,
        Left,
        NoEdges,
        Right,
        LowerLeft,
        Lower,
        LowerRight,
    }
}

impl From<EdgeType> for usize {
    fn from(edge_type: EdgeType) -> Self {
        let mut output = Self::MAX;

        for i in 0..EdgeType::COUNT {
            if EdgeType::ALL[i] == edge_type {
                output = i;
                break;
            }
        }

        output
    }
}

pub use stbhw::BYTES_PER_PIXEL;
const BLUE_PIXEL_OFFSET: usize = 2;

pub const TILES_PER_HW_SHORT_SIDE_U32: u32 = 1 << 3;
pub const TILES_PER_HW_HALF_TILE_U32: u32 = TILES_PER_HW_SHORT_SIDE_U32 * TILES_PER_HW_SHORT_SIDE_U32;

pub const TILES_PER_HW_SHORT_SIDE: usize = TILES_PER_HW_SHORT_SIDE_U32 as _;
#[allow(unused)]
pub const TILES_PER_HW_HALF_TILE: usize = TILES_PER_HW_HALF_TILE_U32 as _;

const T1_SIZE_BYTES: &[u8] = include_bytes!("t1_size.bin");
const T1_SIZE: stbhw::ImageSize = size_from_bytes_or_minus_one(T1_SIZE_BYTES);
const T1_PIXELS: &[u8] = include_bytes!("t1_pixels.bin");

const D1_SIZE_BYTES: &[u8] = include_bytes!("d1_size.bin");
#[allow(unused)]
const D1_SIZE: stbhw::ImageSize = size_from_bytes_or_minus_one(D1_SIZE_BYTES);
const D1_PIXELS: &[u8] = include_bytes!("d1_pixels.bin");

const D1_CARD_SIZE: stbhw::ImageSize = stbhw::ImageSize {
    w: TILES_PER_HW_SHORT_SIDE_U32 as _,
    h: TILES_PER_HW_SHORT_SIDE_U32 as _,
};

pub const D1_CARD_W: usize = D1_CARD_SIZE.w as _;
pub const D1_CARD_H: usize = D1_CARD_SIZE.h as _;

const D1_CARD_W_BYTES: usize = D1_CARD_W * BYTES_PER_PIXEL as usize;
#[allow(unused)]
const D1_CARD_H_BYTES: usize = D1_CARD_H * BYTES_PER_PIXEL as usize;

const D1_CARD_LENGTH: usize = D1_CARD_SIZE.pixels_len();
const D1_CARD_ROW_LENGTH_BYTES: usize = D1_CARD_W * EdgeType::COUNT * BYTES_PER_PIXEL as usize;
const D1_ROW_OF_CARDS_LENGTH: usize = D1_CARD_LENGTH * EdgeType::COUNT;
const D1_LENGTH: usize = D1_PIXELS.len();
const D1_CARD_COUNT: usize = D1_LENGTH / D1_CARD_LENGTH;
const D1_CARD_ROW_COUNT: usize = D1_CARD_COUNT / EdgeType::COUNT;

/// We use a distinct type for Deck1 to make adding different deck types with
/// different sizes later, easier.
pub type Deck1 = [u8; D1_LENGTH];

/// We use a distinct type for Deck1Card to make adding different deck types with
/// different card sizes later, easier.
pub type Deck1Card = [TileKind; D1_CARD_LENGTH];

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

        compile_time_assert!{
            D1_SIZE.w > 0 && D1_SIZE.h > 0
        }
        compile_time_assert!{
            stbhw::Template::pixel_len_matches(D1_SIZE, D1_PIXELS)
        }
        // This asserts that we didn't have an undersized last row of cards.
        compile_time_assert!{
            D1_CARD_COUNT * D1_CARD_LENGTH == D1_LENGTH
        }

        let mut d1 = [0; D1_LENGTH];

        d1.copy_from_slice(D1_PIXELS);

        Self {
            t1,
            d1,
        }
    }
}

impl Templates {
    pub fn d1_card(&self, map_pixel: &[u8], edge_type: EdgeType) -> Deck1Card {
        let card_row = map_pixel[BLUE_PIXEL_OFFSET];

        // For each map_tile_type there are expected to be EdgeType::COUNT
        // cards, one for each edge type.
        let upper_left_corner = (card_row as usize % D1_CARD_ROW_COUNT)
            * D1_ROW_OF_CARDS_LENGTH
            + usize::from(edge_type) * D1_CARD_W_BYTES;

        let mut output: Deck1Card = [<_>::default(); D1_CARD_LENGTH];

        let mut i = 0;
        for y in 0..D1_CARD_H {
            for x in 0..D1_CARD_W {
                use TileKind::*;

                let card_i = y * D1_CARD_ROW_LENGTH_BYTES
                    + x * BYTES_PER_PIXEL as usize;
                let chunk_i = upper_left_corner + card_i;

                output[i] = match self.d1[chunk_i + BLUE_PIXEL_OFFSET] {
                    1 => Wall,
                    2 => Npc,
                    _ => Floor,
                };

                i += 1;
            }
        }

        output
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