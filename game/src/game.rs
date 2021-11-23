#![deny(unused)]
#![deny(bindings_with_variant_name)]

macro_rules! compile_time_assert {
    ($assertion: expr) => {
        #[allow(unknown_lints)]
        // Based on the const_assert macro from static_assertions;
        const _: [(); 0 - !{$assertion} as usize] = [];
    }
}

// In case we decide that we care about no_std/not directly allocating ourself
pub trait ClearableStorage<A> {
    fn clear(&mut self);

    fn push(&mut self, a: A);
}

pub type Seed = [u8; 16];

type Xs = [core::num::Wrapping<u32>; 4];

fn xorshift(xs: &mut Xs) -> u32 {
    let mut t = xs[3];

    xs[3] = xs[2];
    xs[2] = xs[1];
    xs[1] = xs[0];

    t ^= t << 11;
    t ^= t >> 8;
    xs[0] = t ^ xs[0] ^ (xs[0] >> 19);

    xs[0].0
}

#[allow(unused)]
fn xs_u32(xs: &mut Xs, min: u32, one_past_max: u32) -> u32 {
    (xorshift(xs) % (one_past_max - min)) + min
}

#[allow(unused)]
fn xs_shuffle<A>(rng: &mut Xs, slice: &mut [A]) {
    for i in 1..slice.len() as u32 {
        // This only shuffles the first u32::MAX_VALUE - 1 elements.
        let r = xs_u32(rng, 0, i + 1) as usize;
        let i = i as usize;
        slice.swap(i, r);
    }
}

#[allow(unused)]
fn new_seed(rng: &mut Xs) -> Seed {
    let s0 = xorshift(rng).to_le_bytes();
    let s1 = xorshift(rng).to_le_bytes();
    let s2 = xorshift(rng).to_le_bytes();
    let s3 = xorshift(rng).to_le_bytes();

    [
        s0[0], s0[1], s0[2], s0[3],
        s1[0], s1[1], s1[2], s1[3],
        s2[0], s2[1], s2[2], s2[3],
        s3[0], s3[1], s3[2], s3[3],
    ]
}

fn xs_from_seed(mut seed: Seed) -> Xs {
    // 0 doesn't work as a seed, so use this one instead.
    if seed == [0; 16] {
        seed = 0xBAD_5EED_u128.to_le_bytes();
    }

    macro_rules! wrap {
        ($i0: literal, $i1: literal, $i2: literal, $i3: literal) => {
            core::num::Wrapping(
                u32::from_le_bytes([
                    seed[$i0],
                    seed[$i1],
                    seed[$i2],
                    seed[$i3],
                ])
            )
        }
    }

    [
        wrap!( 0,  1,  2,  3),
        wrap!( 4,  5,  6,  7),
        wrap!( 8,  9, 10, 11),
        wrap!(12, 13, 14, 15),
    ]
}

#[test]
fn xs_bad_seed() {
    let mut rng = xs_from_seed(<_>::default());

    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 195911576);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 195911405);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 195911576);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 1788228150);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 195490147);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 1788714188);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 195911576);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 3497122649);
}

#[test]
fn xs_set_seed() {
    let mut rng = xs_from_seed([
        0x12, 0x34, 0x56, 0x78,
        0x90, 0xAB, 0xCD, 0xEF,
        0x12, 0x34, 0x56, 0x78,
        0x90, 0xAB, 0xCD, 0xEF,
    ]);

    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 4198859171);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 863685725);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 2976477321);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 2018915346);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 2932402722);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 1261814658);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 2971803477);
    assert_eq!(xs_u32(&mut rng, 0, u32::MAX), 2015189959);
}

/// This type alias makes adding a custom newtype easy.
pub type X = f32;
/// This type alias makes adding a custom newtype easy.
pub type Y = f32;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct XY {
    pub x: X,
    pub y: Y,
}

pub mod draw;

pub use draw::{
    DrawLength,
    DrawX,
    DrawY,
    DrawXY,
    DrawW,
    DrawH,
    DrawWH,
    SpriteKind,
    SpriteSpec,
    Sizes,
};

macro_rules! counted_enum_def {
    ($name: ident { $( $variants: ident ),+ $(,)? }) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum $name {
            $( $variants ),+
        }

        impl $name {
            pub const COUNT: usize = {
                let mut count = 0;

                $(
                    // Some reference to the vars is needed to use
                    // the repetitions.
                    let _ = Self::$variants;

                    count += 1;
                )+

                count
            };

            pub const ALL: [Self; Self::COUNT] = [
                $(Self::$variants,)+
            ];
        }
    }
}

macro_rules! from_rng_enum_def {
    ($name: ident { $( $variants: ident ),+ $(,)? }) => {
        counted_enum_def!{
            $name {
                $( $variants ),+
            }
        }

        impl $name {
            #[allow(unused)]
            pub fn from_rng(rng: &mut Xs) -> Self {
                Self::ALL[xs_u32(rng, 0, Self::ALL.len() as u32) as usize]
            }
        }
    }
}

from_rng_enum_def!{
    ArrowKind {
        Red,
        Green
    }
}

impl Default for ArrowKind {
    fn default() -> Self {
        Self::Red
    }
}

from_rng_enum_def!{
    Dir {
        Up,
        UpRight,
        Right,
        DownRight,
        Down,
        DownLeft,
        Left,
        UpLeft,
    }
}

impl Default for Dir {
    fn default() -> Self {
        Self::Up
    }
}

mod tile {
    use crate::{Xs, xs_u32};

    pub type Count = u32;

    pub type Coord = u8;

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub struct X(Coord);

    impl X {
        pub const MAX: Coord = 0b0011_1111;
        pub const COUNT: Count = (X::MAX as Count) + 1;

        pub fn from_rng(rng: &mut Xs) -> Self {
            Self(xs_u32(rng, 0, Self::COUNT) as Coord)
        }

        pub fn saturating_add_one(&self) -> Self {
            Self(core::cmp::min(self.0.saturating_add(1), Self::MAX))
        }

        pub fn saturating_sub_one(&self) -> Self {
            Self(self.0.saturating_sub(1))
        }
    }

    impl From<X> for Coord {
        fn from(X(c): X) -> Self {
            c
        }
    }

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub struct Y(Coord);

    impl Y {
        pub const MAX: Coord = 0b0011_1111;
        pub const COUNT: Count = (Y::MAX as Count) + 1;

        pub fn from_rng(rng: &mut Xs) -> Self {
            Self(xs_u32(rng, 0, Self::COUNT) as Coord)
        }

        pub fn saturating_add_one(&self) -> Self {
            Self(core::cmp::min(self.0.saturating_add(1), Self::MAX))
        }

        pub fn saturating_sub_one(&self) -> Self {
            Self(self.0.saturating_sub(1))
        }
    }

    impl From<Y> for Coord {
        fn from(Y(c): Y) -> Self {
            c
        }
    }

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub struct XY {
        pub x: X,
        pub y: Y,
    }

    impl XY {
        pub const COUNT: Count = X::COUNT * Y::COUNT;

        pub fn from_rng(rng: &mut Xs) -> Self {
            Self {
                x: X::from_rng(rng),
                y: Y::from_rng(rng),
            }
        }

        pub fn move_up(&mut self) {
            self.y = self.y.saturating_sub_one();
        }

        pub fn move_down(&mut self) {
            self.y = self.y.saturating_add_one();
        }

        pub fn move_left(&mut self) {
            self.x = self.x.saturating_sub_one();
        }

        pub fn move_right(&mut self) {
            self.x = self.x.saturating_add_one();
        }
    }

    impl XY {
        pub fn is_adjacent_to(&self, xy: Self) -> bool {
            // Who cares about a few extra clones here?
            macro_rules! check {
                ($($method: ident),+ $(,)?) => {
                    let mut current = xy.clone();
                    $(current.$method();)*
                    if current == *self { return true }
                }
            }
            check!(move_up, move_left);
            check!(move_up);
            check!(move_up, move_right);
            check!(move_left);
            check!(move_right);
            check!(move_down, move_left);
            check!(move_down);
            check!(move_down, move_right);

            false
        }
    }

    #[test]
    fn is_adjacent_to_works_on_this_example() {
        let x0 = X::default();
        let x1 = x0.saturating_add_one();
        let x2 = x1.saturating_add_one();

        let y0 = Y::default();
        let y1 = y0.saturating_add_one();
        let y2 = y1.saturating_add_one();

        let xy = XY { x: x1, y: y1 };

        assert!(xy.is_adjacent_to(XY { x: x0, y: y0 }));
        assert!(xy.is_adjacent_to(XY { x: x1, y: y0 }));
        assert!(xy.is_adjacent_to(XY { x: x2, y: y0 }));

        assert!(xy.is_adjacent_to(XY { x: x0, y: y1 }));
        assert!(!xy.is_adjacent_to(xy));
        assert!(xy.is_adjacent_to(XY { x: x2, y: y1 }));

        assert!(xy.is_adjacent_to(XY { x: x0, y: y2 }));
        assert!(xy.is_adjacent_to(XY { x: x1, y: y2 }));
        assert!(xy.is_adjacent_to(XY { x: x2, y: y2 }));

        assert!(!xy.is_adjacent_to(XY { x: x2.saturating_add_one(), y: y2 }));
    }

    #[allow(unused)]
    pub fn xy_to_i(xy: XY) -> usize {
        xy_to_i_usize((usize::from(xy.x.0), usize::from(xy.y.0)))
    }

    pub fn xy_to_i_usize((x, y): (usize, usize)) -> usize {
        y * Y::COUNT as usize + x
    }

    pub fn i_to_xy(index: usize) -> XY {
        XY {
            x: X(to_coord_or_default(
                (index % X::COUNT as usize) as Count
            )),
            y: Y(to_coord_or_default(
                ((index % (XY::COUNT as usize) as usize)
                / X::COUNT as usize) as Count
            )),
        }
    }

    fn to_coord_or_default(n: Count) -> Coord {
        core::convert::TryFrom::try_from(n).unwrap_or_default()
    }
}

fn draw_xy_from_tile(sizes: &Sizes, txy: tile::XY) -> DrawXY {
    DrawXY {
        x: sizes.board_xywh.x + sizes.board_xywh.w * (tile::Coord::from(txy.x) as DrawLength / tile::X::COUNT as DrawLength),
        y: sizes.board_xywh.y + sizes.board_xywh.h * (tile::Coord::from(txy.y) as DrawLength / tile::Y::COUNT as DrawLength),
    }
}

from_rng_enum_def!{
    WallStyle {
        Smooth,
        Rivet,
    }
}

impl Default for WallStyle {
    fn default() -> Self {
        Self::Smooth
    }
}

from_rng_enum_def!{
    WallColour {
        DarkBrown,
        White,
        Grey,
        Black,
        Pink,
        Red,
        DarkRed,
        Yellow,
        Orange,
        Brown,
    }
}

impl Default for WallColour {
    fn default() -> Self {
        Self::DarkBrown
    }
}

#[derive(Copy, Clone, Debug)]
enum SpriteKindSpec {
    #[allow(unused)]
    Arrow,
    #[allow(unused)]
    Wall,
}

impl SpriteKind {
    #[allow(unused)]
    fn from_rng(rng: &mut Xs, spec: SpriteKindSpec) -> Self {
        use SpriteKindSpec::*;
        match spec {
            Arrow => {
                SpriteKind::Arrow(Dir::from_rng(rng), ArrowKind::from_rng(rng))
            },
            Wall => {
                SpriteKind::Wall(WallStyle::from_rng(rng), WallColour::from_rng(rng))
            }
        }
    }
}

from_rng_enum_def!{
    TileKind {
        Floor,
        Wall,
    }
}

impl Default for TileKind {
    fn default() -> Self {
        Self::Floor
    }
}

/// A Tile should always be at a particular position, but that position should be
/// derivable from the tiles location in the tiles array, so it doesn't need to be
/// stored. But, we often want to get the tile's data and it's location as a single
/// thing. This is why we have both `Tile` and `TileData`
#[derive(Copy, Clone, Debug, Default)]
struct TileData {
    kind: TileKind,
}

impl TileData {
    fn sprite(&self) -> SpriteKind {
        use TileKind::*;
        match self.kind {
            Floor => SpriteKind::Floor,
            Wall => SpriteKind::Wall(<_>::default(), <_>::default()),
        }
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct Tile {
    xy: tile::XY,
    data: TileData
}

pub const TILES_WIDTH: usize = tile::X::COUNT as _;
pub const TILES_LENGTH: usize = tile::XY::COUNT as _;

type TileDataArray = [TileData; TILES_LENGTH as _];

use stbhw::{ImageSize, Tileset};
const CHUNK_SIZE: ImageSize = ImageSize {
    w: (tile::X::COUNT / TILES_PER_HW_SHORT_SIDE_U32) as _,
    h: (tile::Y::COUNT / TILES_PER_HW_SHORT_SIDE_U32) as _
};

const TILE_GROUP_W: usize = CHUNK_SIZE.w as _;
const TILE_GROUP_H: usize = CHUNK_SIZE.h as _;
const TILE_GROUP_COUNT: usize = TILE_GROUP_W * TILE_GROUP_H;

/// Given that all items come from NPCs, and at most 1 item is received from each
/// NPC, then the maximum possible number of items per chunk is loosely bounded by
/// the MAX_NPCS_PER_CHUNK. The reserved NO_ITEM does count too, bu
const MAX_ITEM_TYPES_PER_CHUNK: usize = MAX_NPCS_PER_CHUNK;

pub type ItemId = u8;
compile_time_assert!(ItemId::MAX as usize >= MAX_ITEM_TYPES_PER_CHUNK);

pub const NO_ITEM: ItemId = 0;
pub const FIRST_ITEM_ID: ItemId = 1;
/// No `- 1` since the reserved `NO_ITEM` cancels it out.
pub const LAST_ITEM_ID: ItemId = MAX_ITEM_TYPES_PER_CHUNK as ItemId;
const THE_MACGUFFIN: ItemId = LAST_ITEM_ID;

type InventoryBits = u128;

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
struct Inventory {
    bits: InventoryBits,
}

compile_time_assert!(InventoryBits::BITS as usize >= MAX_ITEM_TYPES_PER_CHUNK);

impl Inventory {
    fn contains(&self, id: ItemId) -> bool {
        compile_time_assert!(NO_ITEM == 0);
        id != NO_ITEM && self.bits & (1 << (id - 1)) != 0
    }
}

const MAX_WANT_COUNT: usize = 2;

fn populate_npcs(rng: &mut Xs, active_npcs: &mut[Npc]) {
    let len = active_npcs.len();
    debug_assert!(len <= MAX_NPCS_PER_CHUNK);

    let mut items = [NO_ITEM; MAX_NPCS_PER_CHUNK];
    compile_time_assert!(MAX_NPCS_PER_CHUNK - 1 <= ItemId::MAX as usize);
    for (i, item) in items.iter_mut().enumerate() {
        *item = i as ItemId;
    }
    xs_shuffle(rng, &mut items);
    // Since we manually set `trades[trade_i]` to offer `THE_MACGUFFIN`, `items[0]`
    // will never be read after this point.

    let mut trades = [Trade::default(); MAX_NPCS_PER_CHUNK];
    let mut trade_i = 0;
    trades[trade_i] = Trade {
        wants: [NO_ITEM; MAX_WANT_COUNT],
        offer: THE_MACGUFFIN,
    };
    trade_i += 1;

    while trade_i < len {
        let added_item = items[trade_i];

        let (extended_trade_i, want_i) = {
            let offset = xs_u32(rng, 0, trade_i as u32) as usize;

            let mut extended_trade_i = 0;
            let mut want_i = 0;
            let mut found = false;

            'outer: for base_i in 0..trade_i {
                let randomized_i = (base_i + offset) % trade_i;

                for i in 0..MAX_WANT_COUNT {
                    if trades[randomized_i].wants[i] == NO_ITEM {
                        extended_trade_i = randomized_i;
                        want_i = i;
                        found = true;
                        break 'outer;
                    }
                }
            }

            // We start with a set of trades with `MAX_WANT_COUNT` `NO_ITEM` wants,
            // and each time we add a trade we only set one of those to a random
            // item, so this should always succeed, unless there is a bug.
            assert!(found);

            (extended_trade_i, want_i)
        };

        trades[extended_trade_i].wants[want_i] = added_item;

        trades[trade_i] = Trade {
            wants: [NO_ITEM; MAX_WANT_COUNT],
            offer: added_item,
        };
        trade_i += 1;
    }

    for i in 0..active_npcs.len() {
        active_npcs[i] = Npc::Trade(trades[i]);
    }

    xs_shuffle(rng, active_npcs);
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Trade {
    wants: [ItemId; MAX_WANT_COUNT],
    offer: ItemId
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Npc {
    Nobody,
    Trade(Trade)
}

impl Default for Npc {
    fn default() -> Self {
        Self::Nobody
    }
}

const MAX_NPCS_PER_GROUP: u8 = 2;
const MAX_NPCS_PER_CHUNK: usize = TILE_GROUP_COUNT * MAX_NPCS_PER_GROUP as usize;

#[derive(Clone, Debug)]
pub struct Tiles {
    tiles: TileDataArray,
}

impl Default for Tiles {
    fn default() -> Self {
        Self {
            tiles: [TileData::default(); TILES_LENGTH as _],
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EyeState {
    Idle,
    Moved(Dir),
    NarrowAnimLeft,
    NarrowAnimCenter,
    NarrowAnimRight,
    SmallPupil,
    Closed,
    HalfLid,
}

impl Default for EyeState {
    fn default() -> Self {
        Self::Idle
    }
}

impl EyeState {
    fn sprite(&self) -> SpriteKind {
        use EyeState::*;
        match self {
            Idle => SpriteKind::NeutralEye,
            Moved(dir) => SpriteKind::DirEye(*dir),
            NarrowAnimLeft => SpriteKind::NarrowLeftEye,
            NarrowAnimCenter => SpriteKind::NarrowCenterEye,
            NarrowAnimRight => SpriteKind::NarrowRightEye,
            SmallPupil => SpriteKind::SmallPupilEye,
            Closed => SpriteKind::ClosedEye,
            HalfLid => SpriteKind::HalfLidEye,
        }
    }

    fn idle_tick(&mut self, animation_timer: AnimationTimer) {
        use EyeState::*;
        match *self {
            Idle => {
                if animation_timer % (HOLD_FRAMES * 3) == 0 {
                    *self = NarrowAnimCenter;
                }
            },
            Moved(_) => {
                if animation_timer % HOLD_FRAMES == 0 {
                    *self = Idle;
                }
            },
            SmallPupil => {
                if animation_timer % (HOLD_FRAMES * 3) == 0 {
                    *self = Closed;
                }
            },
            Closed => {
                if animation_timer % (HOLD_FRAMES) == 0 {
                    *self = HalfLid;
                }
            },
            HalfLid => {
                if animation_timer % (HOLD_FRAMES * 5) == 0 {
                    *self = Idle;
                }
            },
            NarrowAnimCenter => {
                let modulus = animation_timer % (HOLD_FRAMES * 4);
                if modulus == 0 {
                    *self = NarrowAnimRight;
                } else if modulus == HOLD_FRAMES * 2 {
                    *self = NarrowAnimLeft;
                }
            },
            NarrowAnimLeft | NarrowAnimRight => {
                if animation_timer % HOLD_FRAMES == 0 {
                    *self = NarrowAnimCenter;
                }
            },
        }
    }

    fn prod(&mut self) {
        *self = Self::SmallPupil;
    }
}

// 64k entities ought to be enough for anybody!
type Entity = u16;

const NPC_ENTITY_MIN: Entity = 0;
const NPC_ENTITY_MAX: Entity = (MAX_NPCS_PER_CHUNK as Entity) - 1;
compile_time_assert!{ MAX_NPCS_PER_CHUNK < Entity::MAX as usize }
const PLAYER_ENTITY: Entity = NPC_ENTITY_MAX + 1;
const ENTITY_COUNT: usize = PLAYER_ENTITY as usize + 1;

macro_rules! component_def {
    ($name: ident([$type: path; ENTITY_COUNT])) => {
        #[derive(Debug)]
        struct $name([$type; ENTITY_COUNT]);

        impl Default for $name {
            fn default() -> Self {
                Self([<_>::default(); ENTITY_COUNT])
            }
        }

        impl core::ops::Index<Entity> for $name {
            type Output = $type;

            fn index(&self, entity: Entity) -> &Self::Output {
                self.0.index(entity as usize)
            }
        }

        impl core::ops::IndexMut<Entity> for $name {
            fn index_mut(&mut self, entity: Entity) -> &mut Self::Output {
                self.0.index_mut(entity as usize)
            }
        }

        impl $name {
            #[allow(unused)]
            fn iter_mut(&mut self) -> impl Iterator<Item = &mut $type> {
                self.0.iter_mut()
            }
        }
    }
}

component_def!{
    EyeStates([EyeState; ENTITY_COUNT])
}

component_def!{
    XYs([tile::XY; ENTITY_COUNT])
}

#[derive(Debug)]
struct Npcs([Npc; MAX_NPCS_PER_CHUNK]);

impl Default for Npcs {
    fn default() -> Self {
        Self([<_>::default(); MAX_NPCS_PER_CHUNK])
    }
}

impl core::ops::Index<usize> for Npcs {
    type Output = Npc;

    fn index(&self, i: usize) -> &Self::Output {
        self.0.index(i)
    }
}

impl core::ops::IndexMut<usize> for Npcs {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        self.0.index_mut(i)
    }
}

#[derive(Debug, Default)]
struct Board {
    rng: Xs,
    tiles: Tiles,
    npcs: Npcs,
    xys: XYs,
    eye_states: EyeStates,
    inventory: Inventory
}

macro_rules! player_xy {
    ($board: expr) => {
        $board.xys[PLAYER_ENTITY]
    }
}

impl Board {
    fn from_seed(seed: Seed, templates: &mut Templates) -> Self {
        let mut rng = xs_from_seed(seed);

        let mut xys = XYs::default();
        xys[PLAYER_ENTITY] = tile::XY::from_rng(&mut rng);

        stbhw::xs_seed_global(new_seed(&mut rng));

        compile_time_assert!{
            CHUNK_SIZE.pixels_len() / stbhw::BYTES_PER_PIXEL as usize
            == TILES_LENGTH / TILES_PER_HW_HALF_TILE
        }

        let mut tileset = Tileset::from_template(&mut templates.t1).unwrap();

        let map = tileset.generate_image(CHUNK_SIZE).unwrap().pixels;

        let mut tiles = [TileData::default(); TILES_LENGTH as _];
        let mut npcs = Npcs::default();
        let mut next_npc_index = NPC_ENTITY_MIN;
        // We currently rely on the coversion from entiy to NPC index being merely
        // a cast.
        compile_time_assert!(NPC_ENTITY_MIN == 0);

        for (pixel_i, chunk) in map.chunks_exact(templates::BYTES_PER_PIXEL as _).enumerate() {
            use EdgeType::*;

            // We want to map a single pixel to multiple tiles. We can picture this
            // as a large pixel grid with tiles inside it.

            let (px, py): (usize, usize) = (
                pixel_i % CHUNK_SIZE.w as usize,
                pixel_i / CHUNK_SIZE.w as usize
            );

            let edge_type = match (px, py) {
                (0, 0) => UpperLeft,
                (x, 0) if x < (CHUNK_SIZE.w - 1) as usize => Upper,
                (_, 0) => UpperRight,
                (0, y) if y < (CHUNK_SIZE.h - 1) as usize => Left,
                (x, y) if
                    y < (CHUNK_SIZE.h - 1) as usize
                    && x < (CHUNK_SIZE.w - 1) as usize => NoEdges,
                (_, y) if y < (CHUNK_SIZE.h - 1) as usize => Right,
                (0, _) => LowerLeft,
                (x, _) if x < (CHUNK_SIZE.w - 1) as usize => Lower,
                (_, _) => LowerRight,
            };

            let card = templates.d1_card(
                chunk,
                edge_type
            );

            let upper_left_corner: usize =
                (py * TILES_WIDTH * TILES_PER_HW_SHORT_SIDE)
                + px * TILES_PER_HW_SHORT_SIDE;

            let mut pushed_npc_count: u8 = 0;
            for y in 0..templates::D1_CARD_H {
                for x in 0..templates::D1_CARD_W {
                    let card_i = y * templates::D1_CARD_W + x;
                    let tile_i = upper_left_corner + y * TILES_WIDTH + x;

                    use TileKind::*;

                    let kind = match card[card_i] {
                        templates::TileKind::Floor => Floor,
                        templates::TileKind::Wall => Wall,
                        templates::TileKind::Npc => {
                            if pushed_npc_count < MAX_NPCS_PER_GROUP
                            // TODO Does this bias towards particular patterns?
                            && xs_u32(&mut rng, 0, 4) == 0 {
                                compile_time_assert!(NPC_ENTITY_MAX < Entity::MAX);
                                if next_npc_index <= NPC_ENTITY_MAX
                                && npcs[next_npc_index as _] == Npc::Nobody {
                                    xys[next_npc_index] = tile::i_to_xy(tile_i);

                                    next_npc_index = next_npc_index.saturating_add(1);
                                }
                                pushed_npc_count += 1;
                            }


                            Floor
                        }
                    };

                    tiles[tile_i] = TileData{
                        kind,
                    }
                }
            }
        }

        let active_npcs = &mut npcs.0[0..next_npc_index as usize];

        populate_npcs(&mut rng, active_npcs);

        Self {
            rng,
            tiles: Tiles { tiles },
            npcs,
            xys,
            .. <_>::default()
        }
    }
}

impl Board {
    fn in_wall(&self, xy: tile::XY) -> bool {
        self.tiles.tiles[tile::xy_to_i(xy)].kind == TileKind::Wall
    }

    fn is_walkable(&self, xy: tile::XY) -> bool {
        if self.tiles.tiles[tile::xy_to_i(xy)].kind == TileKind::Floor {
            for i in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
                match self.npcs[i as _] {
                    Npc::Nobody => {},
                    Npc::Trade(_) => if self.xys[i] == xy {
                        return false;
                    },
                }
            }

            true
        } else {
            false
        }
    }
}

type MoveVariant = u8;

const MAX_MOVE_VARIANT: MoveVariant = 2;

fn move_xy(xy: &mut tile::XY, dir: Dir, variant: MoveVariant) {
    use Dir::*;

    match dir {
        Up => {
            xy.move_up();
        },
        UpRight => {
            // TODO Prefer either up or right based on whether the user inputted Up
            // first or Right first? And do the same for DownRight, etc.
            match variant {
                0 => {
                    xy.move_up();
                    xy.move_right();
                },
                1 => {
                    xy.move_up();
                },
                _ => {
                    xy.move_right();
                }
            }
        },
        Right => {
            xy.move_right();
        },
        DownRight => {
            match variant {
                0 => {
                    xy.move_down();
                    xy.move_right();
                },
                1 => {
                    xy.move_down();
                },
                _ => {
                    xy.move_right();
                }
            }
        },
        Down => {
            xy.move_down();
        },
        DownLeft => {
            match variant {
                0 => {
                    xy.move_down();
                    xy.move_left();
                },
                1 => {
                    xy.move_down();
                },
                _ => {
                    xy.move_left();
                }
            }
        },
        Left => {
            xy.move_left();
        },
        UpLeft => {
            match variant {
                0 => {
                    xy.move_up();
                    xy.move_left();
                },
                1 => {
                    xy.move_up();
                },
                _ => {
                    xy.move_left();
                }
            }
        },
    }
}

/// 64k animation frames ought to be enough for anybody!
type AnimationTimer = u16;

/// We use this because it has a lot more varied factors than 65536.
const ANIMATION_TIMER_LENGTH: AnimationTimer = 60 * 60 * 18;

const HOLD_FRAMES: AnimationTimer = 30;

mod templates;
use templates::{
    Templates,
    EdgeType,
    TILES_PER_HW_SHORT_SIDE,
    TILES_PER_HW_HALF_TILE,
    TILES_PER_HW_SHORT_SIDE_U32,
};

#[derive(Debug, Default)]
pub struct State {
    sizes: draw::Sizes,
    board: Board,
    templates: Templates,
    animation_timer: AnimationTimer
}

impl State {
    pub fn from_seed(seed: Seed) -> Self {
        let mut templates = <_>::default();

        Self {
            board: Board::from_seed(seed, &mut templates),
            templates,
            ..<_>::default()
        }
    }
}

pub fn sizes(state: &State) -> draw::Sizes {
    state.sizes.clone()
}

pub type InputFlags = u16;

pub const INPUT_UP_PRESSED: InputFlags              = 0b0000_0000_0000_0001;
pub const INPUT_DOWN_PRESSED: InputFlags            = 0b0000_0000_0000_0010;
pub const INPUT_LEFT_PRESSED: InputFlags            = 0b0000_0000_0000_0100;
pub const INPUT_RIGHT_PRESSED: InputFlags           = 0b0000_0000_0000_1000;

pub const INPUT_UP_DOWN: InputFlags                 = 0b0000_0000_0001_0000;
pub const INPUT_DOWN_DOWN: InputFlags               = 0b0000_0000_0010_0000;
pub const INPUT_LEFT_DOWN: InputFlags               = 0b0000_0000_0100_0000;
pub const INPUT_RIGHT_DOWN: InputFlags              = 0b0000_0000_1000_0000;

pub const INPUT_INTERACT_PRESSED: InputFlags        = 0b0000_0001_0000_0000;

#[derive(Clone, Copy, Debug)]
enum Input {
    NoChange,
    Dir(Dir),
    Interact,
}

impl Input {
    fn from_flags(flags: InputFlags) -> Self {
        use Input::*;
        use crate::Dir::*;
        if INPUT_INTERACT_PRESSED & flags != 0 {
            Interact
        } else if (INPUT_UP_DOWN | INPUT_RIGHT_DOWN) & flags == (INPUT_UP_DOWN | INPUT_RIGHT_DOWN) {
            Dir(UpRight)
        } else if (INPUT_DOWN_DOWN | INPUT_RIGHT_DOWN) & flags == (INPUT_DOWN_DOWN | INPUT_RIGHT_DOWN) {
            Dir(DownRight)
        } else if (INPUT_DOWN_DOWN | INPUT_LEFT_DOWN) & flags == (INPUT_DOWN_DOWN | INPUT_LEFT_DOWN) {
            Dir(DownLeft)
        } else if (INPUT_UP_DOWN | INPUT_LEFT_DOWN) & flags == (INPUT_UP_DOWN | INPUT_LEFT_DOWN) {
            Dir(UpLeft)
        } else if INPUT_UP_DOWN & flags != 0 {
            Dir(Up)
        } else if INPUT_DOWN_DOWN & flags != 0 {
            Dir(Down)
        } else if INPUT_LEFT_DOWN & flags != 0 {
            Dir(Left)
        } else if INPUT_RIGHT_DOWN & flags != 0 {
            Dir(Right)
        } else {
            NoChange
        }
    }
}

pub fn update(
    state: &mut State,
    commands: &mut dyn ClearableStorage<draw::Command>,
    input_flags: InputFlags,
    draw_wh: DrawWH,
) {
    #[allow(unused)]
    use draw::{TextSpec, TextKind, Command::*};

    if draw_wh != state.sizes.draw_wh {
        state.sizes = draw::fresh_sizes(draw_wh);
    }

    commands.clear();

    let input = Input::from_flags(input_flags);

    use EyeState::*;
    use Input::*;

    for e_s in state.board.eye_states.iter_mut() {
        e_s.idle_tick(state.animation_timer);
    }

    match input {
        NoChange => {},
        Dir(dir) => {
            state.board.eye_states[PLAYER_ENTITY] = Moved(dir);

            let pxy = player_xy!(state.board);
            let mut target = pxy;

            for i in 0..=MAX_MOVE_VARIANT {
                move_xy(&mut target, dir, i);

                // Let things already embedded in walls move so they can get out.
                let can_pass = state.board.in_wall(player_xy!(state.board))
                    || state.board.is_walkable(target);
                if can_pass {
                    player_xy!(state.board) = target;
                    break;
                }
                target = pxy;
            }
        },
        Interact => {
            state.board.eye_states[PLAYER_ENTITY].prod();

            let player_xy = player_xy!(state.board);

            for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
                if state.board.xys[entity].is_adjacent_to(player_xy) {
                    state.board.eye_states[entity].prod();
                }
            }
        },
    }

    for i in 0..TILES_LENGTH {
        let tile_data = state.board.tiles.tiles[i];

        let txy = tile::i_to_xy(i);

        commands.push(Sprite(SpriteSpec{
            sprite: tile_data.sprite(),
            xy: draw_xy_from_tile(&state.sizes, txy),
        }));
    }

    for i in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
        match state.board.npcs[i as usize] {
            Npc::Nobody => break,
            Npc::Trade(_) => {
                commands.push(Sprite(SpriteSpec{
                    sprite: state.board.eye_states[i].sprite(),
                    xy: draw_xy_from_tile(&state.sizes, state.board.xys[i]),
                }));
            },
        }
    }

    commands.push(Sprite(SpriteSpec{
        sprite: state.board.eye_states[PLAYER_ENTITY].sprite(),
        xy: draw_xy_from_tile(&state.sizes, player_xy!(state.board)),
    }));

    type PreDrawUint = u16;
    const INVENTORY_COLUMNS_COUNT: PreDrawUint = 4;
    for i in FIRST_ITEM_ID..=LAST_ITEM_ID {
        if state.board.inventory.contains(i) {
            let inventory_offset = i as PreDrawUint - FIRST_ITEM_ID as PreDrawUint;
            let x_offset = (inventory_offset % INVENTORY_COLUMNS_COUNT)
                // Space the columns a half-tile apart from the grid and a tile apart
                // from each other
                as DrawLength * 2. + 0.5;
            let y_offset = (inventory_offset / INVENTORY_COLUMNS_COUNT)
                // Space the rows a half-tile apart from the top of the grid and a
                // tile apart from each other
                as DrawLength * 2. + 0.5;
            let xy = DrawXY {
                x: state.sizes.board_xywh.x + state.sizes.board_xywh.w
                    + (x_offset * state.sizes.tile_side_length),
                y: state.sizes.board_xywh.y
                    + (y_offset * state.sizes.tile_side_length),
            };

            commands.push(Sprite(SpriteSpec{
                sprite: SpriteKind::Item(i as ItemId),
                xy,
            }));
        }
    }

    // TODO make this debug text toggleable?
    /*
    let left_text_x = state.sizes.play_xywh.x + MARGIN;

    const MARGIN: f32 = 16.;

    let small_section_h = state.sizes.draw_wh.h / 8. - MARGIN;

    {
        let mut y = MARGIN;

        commands.push(Text(TextSpec{
            text: format!(
                "input: {:?}",
                input
            ),
            xy: DrawXY { x: left_text_x, y },
            wh: DrawWH {
                w: state.sizes.play_xywh.w,
                h: small_section_h
            },
            kind: TextKind::UI,
        }));

        y += small_section_h;

        commands.push(Text(TextSpec{
            text: format!(
                "sizes: {:?}\nanimation_timer: {:?}",
                state.sizes,
                state.animation_timer
            ),
            xy: DrawXY { x: left_text_x, y },
            wh: DrawWH {
                w: state.sizes.play_xywh.w,
                h: state.sizes.play_xywh.h - y
            },
            kind: TextKind::UI,
        }));
    }
    */

    state.animation_timer += 1;
    if state.animation_timer >= ANIMATION_TIMER_LENGTH {
        state.animation_timer = 0;
    }
}
