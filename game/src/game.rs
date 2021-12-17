#![deny(unused)]
#![deny(bindings_with_variant_name)]
#![deny(clippy::unused_self)]

macro_rules! compile_time_assert {
    ($assertion: expr) => {
        #[allow(unknown_lints)]
        // Based on the const_assert macro from static_assertions;
        const _: [(); 0 - !{$assertion} as usize] = [];
    }
}

use std::collections::HashMap;

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
    EyeSpriteKind,
    EyeVariant,
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

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    struct XYOrthoganalIter{
        xys: [Option<XY>; crate::Dir::COUNT],
        index: usize
    }

    impl XYOrthoganalIter {
        #[allow(unused_assignments)]
        fn new(xy: XY) -> Self {
            let mut xys = [None; crate::Dir::COUNT];
            compile_time_assert!(crate::Dir::COUNT < usize::MAX);

            let mut i = 0;
            macro_rules! push {
                ($method: ident) => {
                    let mut new_xy = xy;
                    new_xy.$method();
                    if new_xy != xy {
                        xys[i] = Some(new_xy);
                    }
                    // This is unused in the last macro invocation.
                    i += 1;
                }
            }

            push!(move_up);
            push!(move_left);
            push!(move_right);
            push!(move_down);

            XYOrthoganalIter {
                xys,
                index: 0,
            }
        }
    }

    impl Iterator for XYOrthoganalIter {
        type Item = XY;

        fn next(&mut self) -> Option<XY> {
            let output = self.xys.get(self.index).and_then(|xy| xy.map(|xy| xy));
            self.index += 1;
            output
        }
    }

    impl XY {
        pub fn orthogonal_iter(self) -> impl Iterator<Item = Self> {
            XYOrthoganalIter::new(self)
        }

        #[allow(unused)]
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

impl TileKind {
    fn is_walkable(&self) -> bool {
        *self == Self::Floor
    }
}

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
        id != NO_ITEM && self.bits & Self::id_to_bits(id) != 0
    }

    fn id_to_bits(id: ItemId) -> InventoryBits {
        if id == NO_ITEM {
            0
        } else {
            1 << (id - 1)
        }
    }
}

impl Inventory {
    fn insert(&mut self, id: ItemId) {
        self.bits |= Self::id_to_bits(id);
    }

    fn insert_all(&mut self, other: Self) {
        self.bits |= other.bits;
    }

    fn remove(&mut self, id: ItemId) {
        self.bits &= !Self::id_to_bits(id);
    }
}

const UNRESTRICTED_AGENT_COUNT_MIN: u32 = MAX_NPCS_PER_CHUNK as u32 / 16;
const UNRESTRICTED_AGENT_COUNT_MAX: u32 = MAX_NPCS_PER_CHUNK as u32 / 8;
compile_time_assert!(UNRESTRICTED_AGENT_COUNT_MIN <= UNRESTRICTED_AGENT_COUNT_MAX);

fn agent_count_range(npc_count: u32) -> (u32, u32) {
    if npc_count < UNRESTRICTED_AGENT_COUNT_MAX {
        if npc_count < UNRESTRICTED_AGENT_COUNT_MIN {
            (0, 1)
        } else {
            // TODO scale more continuously?
            (UNRESTRICTED_AGENT_COUNT_MIN / 2, UNRESTRICTED_AGENT_COUNT_MAX / 2 + 1)
        }
    } else {
        (UNRESTRICTED_AGENT_COUNT_MIN, UNRESTRICTED_AGENT_COUNT_MAX + 1)
    }
}

#[test]
fn agent_count_range_gives_the_expected_results_for_these_cases() {
    assert_eq!(agent_count_range(UNRESTRICTED_AGENT_COUNT_MAX * 3), (UNRESTRICTED_AGENT_COUNT_MIN, UNRESTRICTED_AGENT_COUNT_MAX + 1));
    assert_eq!(agent_count_range(UNRESTRICTED_AGENT_COUNT_MIN), (UNRESTRICTED_AGENT_COUNT_MIN / 2, (UNRESTRICTED_AGENT_COUNT_MAX / 2) + 1));
    assert_eq!(agent_count_range(0), (0, 1));
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

    debug_assert!(len <= u32::MAX as usize);
    let (agent_count_min, agent_count_one_past_max) = agent_count_range(len as u32);

    let agent_count = xs_u32(
        rng,
        agent_count_min,
        agent_count_one_past_max,
    );

    let mut trades = [Trade::default(); MAX_NPCS_PER_CHUNK];
    let mut trade_i = 0;
    trades[trade_i] = Trade {
        wants: [NO_ITEM; MAX_WANT_COUNT],
        offer: THE_MACGUFFIN,
    };
    trade_i += 1;

    let trader_count = len - agent_count as usize;
    while trade_i < trader_count {
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

    for i in 0..trader_count {
        active_npcs[i] = Npc::Trade(trades[i]);
    }

    for npc in active_npcs.iter_mut().take(len).skip(trader_count) {
        *npc = Npc::Agent(<_>::default());
    }

    xs_shuffle(rng, active_npcs);
}

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
    fn sprite(&self) -> EyeSpriteKind {
        use EyeState::*;
        match self {
            Idle => EyeSpriteKind::NeutralEye,
            Moved(dir) => EyeSpriteKind::DirEye(*dir),
            NarrowAnimLeft => EyeSpriteKind::NarrowLeftEye,
            NarrowAnimCenter => EyeSpriteKind::NarrowCenterEye,
            NarrowAnimRight => EyeSpriteKind::NarrowRightEye,
            SmallPupil => EyeSpriteKind::SmallPupilEye,
            Closed => EyeSpriteKind::ClosedEye,
            HalfLid => EyeSpriteKind::HalfLidEye,
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

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Trade {
    wants: [ItemId; MAX_WANT_COUNT],
    offer: ItemId
}

impl Trade {
    #[allow(unused)]
    fn contains(&self, item_id: ItemId) -> bool {
        item_id != NO_ITEM
        && (
            self.offer == item_id
            || self.wants.iter().any(|&id| id == item_id)
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AgentTarget {
    NoTarget,
    Target(tile::XY),
}

impl Default for AgentTarget {
    fn default() -> Self {
        Self::NoTarget
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Agent {
    inventory: Inventory,
    target: AgentTarget,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Npc {
    Nobody,
    Trade(Trade),
    NoTrade,
    Agent(Agent)
}

impl Default for Npc {
    fn default() -> Self {
        Self::Nobody
    }
}

impl Npc {
    fn is_walkable(&self) -> bool {
        match self {
            Npc::Nobody => true,
            Npc::Trade(_)
            | Npc::NoTrade
            | Npc::Agent(_) => false,
        }
    }
}

const MAX_NPCS_PER_GROUP: u8 = 2;
const MAX_NPCS_PER_CHUNK: usize = TILE_GROUP_COUNT * MAX_NPCS_PER_GROUP as usize;

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

impl core::ops::Index<Entity> for Npcs {
    type Output = Npc;

    fn index(&self, i: Entity) -> &Self::Output {
        self.0.index(i as usize)
    }
}

impl core::ops::IndexMut<Entity> for Npcs {
    fn index_mut(&mut self, i: Entity) -> &mut Self::Output {
        self.0.index_mut(i as usize)
    }
}

#[derive(Debug)]
enum Speech {
    Silence,
    Trade(Trade),
}

impl Default for Speech {
    fn default() -> Self {
        Self::Silence
    }
}

impl From<Npc> for Speech {
    fn from(npc: Npc) -> Self {
        match npc {
            Npc::Nobody
            | Npc::NoTrade
            // Later, we can have the agent make trades too.
            | Npc::Agent(_) => Self::Silence,
            Npc::Trade(trade) => Self::Trade(trade),
        }
    }
}

type RegenTimer = u16;

const REGEN_TIMER_LENGTH: RegenTimer = 3 * 60 /* 60 FPS */ ;

#[derive(Debug, Default)]
struct RegenState {
    item_offset: ItemId,
    npc_offset: Entity,
    timer: RegenTimer,
}

impl RegenState {
    fn regeneratable_trade(
        &mut self,
        inventory: &Inventory,
        npcs: &Npcs,
    ) -> Option<Trade> {
        let mut trade = Trade::default();

        // TODO cache this on the regen state, so we only need to update it when the
        // NPCs have changed.
        let npc_inventory = {
            let mut npc_inventory = Inventory::default();

            for npc in &npcs.0 {
                match npc {
                    Npc::Nobody => break,
                    Npc::Trade(trade) => {
                        npc_inventory.insert(trade.offer);
                        for id in trade.wants {
                            npc_inventory.insert(id);
                        }
                    },
                    Npc::NoTrade => {},
                    Npc::Agent(agent) => {
                        npc_inventory.insert_all(agent.inventory);
                    }
                }
            }

            npc_inventory
        };

        for item_i in FIRST_ITEM_ID..=LAST_ITEM_ID {
            let item_id = (
                (item_i + self.item_offset)
                % (LAST_ITEM_ID - FIRST_ITEM_ID)
            ) + FIRST_ITEM_ID;

            // TODO sometimes regenerate trades with wants.
            if !inventory.contains(item_id)
            && !npc_inventory.contains(item_id)
            {
                trade.offer = item_id;

                // TODO randomize this maybe? Or is this unpredicatable enough?
                self.item_offset += 1;

                return Some(trade);
            }
        }

        None
    }
}

#[derive(Debug, Default)]
struct Board {
    regen: RegenState,
    tiles: Tiles,
    npcs: Npcs,
    xys: XYs,
    eye_states: EyeStates,
    inventory: Inventory,
    speech: Speech,
    rng: Xs,
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
        // We currently rely on the coversion from entity to NPC index being merely
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
                                && npcs[next_npc_index] == Npc::Nobody {
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
            tiles: Tiles { tiles },
            npcs,
            xys,
            rng,
            .. <_>::default()
        }
    }
}

impl Board {
    fn in_wall(&self, xy: tile::XY) -> bool {
        self.tiles.tiles[tile::xy_to_i(xy)].kind == TileKind::Wall
    }

    fn is_walkable(&self, xy: tile::XY) -> bool {
        if self.tiles.tiles[tile::xy_to_i(xy)].kind.is_walkable() {
            for i in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
                match self.npcs[i] {
                    Npc::Nobody => break,
                    Npc::NoTrade
                    | Npc::Trade(_)
                    | Npc::Agent(_) => if self.xys[i] == xy {
                        return false;
                    },
                }
            }

            self.xys[PLAYER_ENTITY] != xy
        } else {
            false
        }
    }

    #[allow(unused)]
    fn walkable_from(&self, at: tile::XY) -> impl Iterator<Item = tile::XY> + '_ {
        at.orthogonal_iter().filter(|&xy| {
            self.is_walkable(xy)
        })
    }
}

type MoveVariant = u8;

const DIRECT_MOVE_VARIANT: MoveVariant = 0;
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

// When/if we have a second use case, maybe make this a generic constant?
// That is, something like `struct XYDeck<const COUNT: usize> {}`
const XY_DECK_COUNT: usize = MAX_NPCS_PER_CHUNK * Dir::COUNT;
struct XYDeck {
    deck: [tile::XY; XY_DECK_COUNT],
    current_index: usize,
    max_index: usize,
}
compile_time_assert!(MAX_NPCS_PER_CHUNK > 0);

impl XYDeck {
    fn active_trading_spots(board: &mut Board) -> Option<XYDeck> {
        let mut deck = [tile::XY::default(); XY_DECK_COUNT];

        let mut max_index = None;

        for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
            if let Npc::Trade(_) = board.npcs[entity] {
                for xy in board.xys[entity].orthogonal_iter() {
                    if board.is_walkable(xy) {
                        let i = max_index.map(|i| i + 1).unwrap_or(0);
                        deck[i] = xy;
                        max_index = Some(i);
                    }
                }
            }
        }

        max_index.map(|max_index| {
            xs_shuffle(&mut board.rng, &mut deck[0..=max_index]);

            XYDeck {
                deck,
                max_index,
                current_index: max_index,
            }
        })
    }

    fn draw(&mut self, rng: &mut Xs) -> tile::XY {
        let output = self.deck[self.current_index];
        if self.current_index == 0 {
            xs_shuffle(rng, &mut self.deck[0..=self.max_index]);
            self.current_index = self.max_index;
        } else {
            self.current_index -= 1;
        }

        output
    }
}

type Distance = tile::Count;

fn manhattan_distance(
    tile::XY{ x: x1, y: y1 }: tile::XY,
    tile::XY{ x: x2, y: y2 }: tile::XY
) -> Distance {
    compile_time_assert!(i16::BITS > tile::Coord::BITS);
    compile_time_assert!(Distance::BITS >= i16::BITS);
    macro_rules! to { ($c: ident) => {{ tile::Coord::from($c) as i16 }} }
    ((to!(x1) - to!(x2)).abs() + (to!(y1) - to!(y2)).abs()) as Distance
}

struct WalkGoal {
    at: tile::XY,
    target: tile::XY,
}

fn next_walk_step(
    is_walkable_map: &IsWalkableMap,
    WalkGoal{at, target}: WalkGoal
) -> tile::XY {
    use std::collections::BinaryHeap;
    use core::cmp::Ordering;

    // The maximum possible length shortest path would be from one corner
    // to the opposite corner, along the edges.
    const LONGEST_PATH_LENGTH: usize = TILES_WIDTH * 2;

    // A* based on the pseudocode at
    // https://en.wikipedia.org/w/index.php?title=A*_search_algorithm&oldid=1055876705
    // The Rust BinaryHeap docs were also referenced.
    // We bake in manhattan_distance as the heuristic function.

    // For a given xy, came_from[xy] is the xy immediately preceding it on the
    // cheapest path from at to xy currently known.
    let mut came_from: HashMap<tile::XY, tile::XY>
        = HashMap::with_capacity(LONGEST_PATH_LENGTH);

    // For a given xy, g_score[xy] is the cost of the cheapest path from at to xy
    // currently known.
    let mut g_score: HashMap<tile::XY, Distance>
        = HashMap::with_capacity(LONGEST_PATH_LENGTH);
    g_score.insert(at, 0);

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    struct ScoredXY {
        // For a ScoredXY with a given xy, we mantain that
        // f_score[xy] := g_score[xy] + manhattan_distance(at, xy).
        // f_score[xy] represents our current best guess as to how short a path from
        // at to target can be if it goes through xy.
        f_score: Distance,
        xy: tile::XY,
    }

    // The priority queue depends on `Ord`.
    // Explicitly implement the trait so the queue becomes a min-heap
    // instead of a max-heap.
    impl Ord for ScoredXY {
        fn cmp(&self, other: &Self) -> Ordering {
            // Notice that the we flip the ordering on scores.
            other.f_score.cmp(&self.f_score)
                // In case of a tie we compare positions - this step is necessary
                // to make implementations of `PartialEq` and `Ord` consistent.
                .then_with(|| self.xy.cmp(&other.xy))
        }
    }

    impl PartialOrd for ScoredXY {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    let mut open_set = BinaryHeap::with_capacity(
        LONGEST_PATH_LENGTH
    );

    open_set.push(ScoredXY{
        xy: at,
        // AKA manhattan_distance(at, at)
        f_score: 0,
    });

    let mut output = at;

    while !open_set.is_empty() {
        // We can unwrap since we just checked if it was empty.
        let current = open_set.peek().cloned().unwrap();
        if current.xy == target {
            output = target;

            while let Some(&v) = came_from.get(&output) {
                if v == at {
                    break
                }
                output = v;
            }
            
            break
        }

        // Peek is O(1), pop is O(log n), so popping after the return saves us from
        // having to do the final O(log n) operations.
        open_set.pop();

        for neighbor in walkable_from_iter(is_walkable_map, current.xy) {
            // tentative_g_score is the distance from start to the neighbor through
            // current
            let tentative_g_score =
                g_score.get(&current.xy)
                .unwrap_or(&Distance::MAX)
                // + 1 for the distance from current to neighbor.
                .saturating_add(1);
            if
                tentative_g_score
                < g_score.get(&neighbor).cloned().unwrap_or(Distance::MAX)
            {
                // This path to neighbor is better than any previous one. Record it!
                came_from.insert(neighbor, current.xy);
                g_score.insert(neighbor, tentative_g_score);

                let mut not_already_there = true;
                for scored_xy in open_set.iter() {
                    if scored_xy.xy == neighbor {
                        not_already_there = false;
                        break
                    }
                }

                if not_already_there {
                    open_set.push(ScoredXY {
                        f_score: tentative_g_score.saturating_add(
                            manhattan_distance(at, neighbor)
                        ),
                        xy: neighbor
                    });
                }
            }
        }
    }

    // If this is still `at`, then the open set is empty but goal was never reached.
    output
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
    animation_timer: AnimationTimer,
    last_dir: Option<Dir>,
}

impl State {
    pub fn from_seed(seed: Seed) -> Self {
        let mut templates = <_>::default();

        Self {
            board: Board::from_seed(seed, &mut templates),
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
//pub const INPUT_WALK_DOWN: InputFlags               = 0b0000_0010_0000_0000;

#[derive(Clone, Copy, Debug)]
enum Input {
    NoChange,
    Dir(Dir),
    Interact,
}

impl Dir {
    fn from_flags(flags: InputFlags) -> Option<Self> {
        use crate::Dir::*;
        if (INPUT_UP_DOWN | INPUT_RIGHT_DOWN) & flags == (INPUT_UP_DOWN | INPUT_RIGHT_DOWN) {
            Some(UpRight)
        } else if (INPUT_DOWN_DOWN | INPUT_RIGHT_DOWN) & flags == (INPUT_DOWN_DOWN | INPUT_RIGHT_DOWN) {
            Some(DownRight)
        } else if (INPUT_DOWN_DOWN | INPUT_LEFT_DOWN) & flags == (INPUT_DOWN_DOWN | INPUT_LEFT_DOWN) {
            Some(DownLeft)
        } else if (INPUT_UP_DOWN | INPUT_LEFT_DOWN) & flags == (INPUT_UP_DOWN | INPUT_LEFT_DOWN) {
            Some(UpLeft)
        } else if INPUT_UP_DOWN & flags != 0 {
            Some(Up)
        } else if INPUT_DOWN_DOWN & flags != 0 {
            Some(Down)
        } else if INPUT_LEFT_DOWN & flags != 0 {
            Some(Left)
        } else if INPUT_RIGHT_DOWN & flags != 0 {
            Some(Right)
        } else {
            None
        }
    }
}

impl Input {
    fn from_flags(flags: InputFlags) -> Self {
        use Input::*;
        if INPUT_INTERACT_PRESSED & flags != 0 {
            Interact
        } else if let Some(dir) = crate::Dir::from_flags(flags) {
            Dir(dir)
        } else {
            NoChange
        }
    }
}

type IsWalkableMap = [bool; TILES_LENGTH];

fn walkable_from_iter(is_walkable_map: &IsWalkableMap, at: tile::XY) -> impl Iterator<Item = tile::XY> + '_ {
    at.orthogonal_iter().filter(|&xy| {
        let i = tile::xy_to_i(xy);

        is_walkable_map[i]
    })
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

    if state.board.regen.timer == 0 {
        if let Some(trade) = state.board.regen.regeneratable_trade(
            &state.board.inventory,
            &state.board.npcs,
         ) {
            for npc_i in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
                let npc_index = (
                    (npc_i + state.board.regen.npc_offset)
                    % (NPC_ENTITY_MAX - NPC_ENTITY_MIN)
                ) + NPC_ENTITY_MIN;

                if let Npc::NoTrade = state.board.npcs[npc_index] {
                    state.board.npcs[npc_index] = Npc::Trade(trade);

                    // TODO randomize this maybe? Or is this unpredicatable enough?
                    state.board.regen.npc_offset += 1;
                    break;
                }
            }
        }
    }

    let input = Input::from_flags(input_flags);

    use EyeState::*;
    use Input::*;

    for e_s in state.board.eye_states.iter_mut() {
        e_s.idle_tick(state.animation_timer);
    }

    match input {
        NoChange => {},
        Dir(dir) => {
            if Some(dir) != state.last_dir {
                state.board.speech = Speech::Silence;
            }
            state.last_dir = Some(dir);

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
            if let Some(dir) = crate::Dir::from_flags(input_flags) {
                let mut target = player_xy!(state.board);
                move_xy(&mut target, dir, DIRECT_MOVE_VARIANT);

                for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
                    if state.board.xys[entity] == target {
                        let target_speech = state.board.npcs[entity].into();

                        state.board.speech = match state.board.speech {
                            Speech::Silence => target_speech,
                            Speech::Trade(trade) => {
                                if trade.wants.iter()
                                    .all(|&want|
                                        want == NO_ITEM
                                        || state.board.inventory.contains(want)
                                    ) {

                                    for &want in trade.wants.iter() {
                                        state.board.inventory.remove(want);
                                    }
                                    state.board.inventory.insert(trade.offer);

                                    // TODO Have them walk somewhere else or
                                    // something?
                                    state.board.npcs[entity] = Npc::NoTrade;
                                }

                                Speech::Silence
                            },
                        };

                        state.board.eye_states[PLAYER_ENTITY].prod();
                        state.board.eye_states[entity].prod();

                        break;
                    }
                }
            }
        },
    }

    if let Some(mut trader_xy_deck) = XYDeck::active_trading_spots(
        &mut state.board,
    ) {
        // TODO is it worth it to make this a per-frame thing? Or maybe store it
        // across frames and update it?
        let mut is_walkable_map: IsWalkableMap = [false; TILES_LENGTH];
        for (i, element) in is_walkable_map.iter_mut().enumerate() {
            *element = state.board.tiles.tiles[i].kind.is_walkable();
        }
        for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
            if !state.board.npcs[entity].is_walkable() {
                let i = tile::xy_to_i(state.board.xys[entity]);
                is_walkable_map[i] = false;
            }
        }
        is_walkable_map[tile::xy_to_i(state.board.xys[PLAYER_ENTITY])] = false;

        let mut move_pairs = Vec::with_capacity((NPC_ENTITY_MAX - NPC_ENTITY_MIN) as usize);

        for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
            if let Npc::Agent(ref mut agent) = state.board.npcs[entity] {
                use AgentTarget::*;
                match agent.target {
                    NoTarget => {
                        // It is, of course, possible that the location will not be
                        // walkable by the time we get there. A form of the TOCTOU
                        // problem. The agent will need to deal with this when they
                        // get closer to there. Similarly they will also need to
                        // deal with the possibility of multiple agents having the
                        // same target, or the trader not being active at that time.
                        agent.target = Target(
                            trader_xy_deck.draw(&mut state.board.rng)
                        );
                    },
                    Target(target) => {
                        let goal = WalkGoal {
                            at: state.board.xys[entity],
                            target,
                        };
                        let next = next_walk_step(
                            &is_walkable_map,
                            goal
                        );

                        move_pairs.push((entity, next));
                    }
                }
            }
        }

        let mut counts: HashMap<tile::XY, _> = HashMap::with_capacity(move_pairs.len());
        for &(_entity, xy) in &move_pairs {
            let counter = counts.entry(xy).or_insert(0);
            *counter += 1;
        }

        for (entity, xy) in move_pairs {
            // We can use [] since we just inserted every xy.
            if counts[&xy] == 1 {
                state.board.xys[entity] = xy;
            }
        }
    }

    //
    // Drawing
    //

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
                    sprite: SpriteKind::Eye(
                        EyeVariant::Trader,
                        state.board.eye_states[i].sprite()
                    ),
                    xy: draw_xy_from_tile(&state.sizes, state.board.xys[i]),
                }));
            },
            Npc::NoTrade => {
                commands.push(Sprite(SpriteSpec{
                    sprite: SpriteKind::Eye(
                        EyeVariant::Trader,
                        EyeSpriteKind::ClosedEye
                    ),
                    xy: draw_xy_from_tile(&state.sizes, state.board.xys[i]),
                }));
            },
            Npc::Agent(agent) => {
                let xy = state.board.xys[i];
                commands.push(Sprite(SpriteSpec{
                    sprite: SpriteKind::Eye(
                        EyeVariant::Agent,
                        state.board.eye_states[i].sprite()
                    ),
                    xy: draw_xy_from_tile(&state.sizes, xy),
                }));

                use AgentTarget::*;
                match agent.target {
                    NoTarget => {},
                    Target(target) => {
                        commands.push(Sprite(SpriteSpec{
                            sprite: SpriteKind::Arrow(
                                <_>::default(),
                                if target == xy {
                                    ArrowKind::Green
                                } else {
                                    ArrowKind::Red
                                }
                            ),
                            xy: draw_xy_from_tile(&state.sizes, target),
                        }));
                    }
                }
            },
        }
    }

    commands.push(Sprite(SpriteSpec{
        sprite: SpriteKind::Eye(
            EyeVariant::Agent,
            state.board.eye_states[PLAYER_ENTITY].sprite()
        ),
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

    const MARGIN: f32 = 16.;

    match state.board.speech {
        Speech::Silence => {},
        Speech::Trade(trade) => {
            let left_edge_x = state.sizes.play_xywh.x + MARGIN;
            let top_edge_y = state.sizes.board_xywh.y + MARGIN;

            let mut x = left_edge_x;

            for i in (0..MAX_WANT_COUNT).rev() {
                commands.push(Sprite(SpriteSpec{
                    sprite: SpriteKind::Item(trade.wants[i]),
                    xy: DrawXY { x, y: top_edge_y },
                }));

                x += MARGIN + state.sizes.tile_side_length;
            }

            commands.push(Sprite(SpriteSpec{
                sprite: SpriteKind::Arrow(crate::Dir::Right, ArrowKind::Green),
                xy: DrawXY { x, y: top_edge_y },
            }));

            x += MARGIN + state.sizes.tile_side_length;

            commands.push(Sprite(SpriteSpec{
                sprite: SpriteKind::Item(trade.offer),
                xy: DrawXY { x, y: top_edge_y },
            }));
        },
    }

    // TODO make this debug text toggleable?
    /*
    let left_text_x = state.sizes.play_xywh.x + MARGIN;

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

    state.board.regen.timer += 1;
    if state.board.regen.timer >= REGEN_TIMER_LENGTH {
        state.board.regen.timer = 0;
    }
}
