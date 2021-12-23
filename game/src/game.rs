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

use xs::{Seed, Xs, new_seed, xs_u32, xs_shuffle, xs_from_seed, counted_enum_def, from_rng_enum_def};
pub use tile::{Dir, TILES_WIDTH, TILES_HEIGHT, TILES_LENGTH};

// In case we decide that we care about no_std/not directly allocating ourself
pub trait ClearableStorage<A> {
    fn clear(&mut self);

    fn push(&mut self, a: A);
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
    LineSpec,
    Sizes,
};

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

fn draw_xy_from_tile(sizes: &Sizes, txy: tile::XY) -> DrawXY {
    DrawXY {
        x: sizes.board_xywh.x + sizes.board_xywh.w * (tile::Coord::from(txy.x) as DrawLength / tile::X::COUNT as DrawLength),
        y: sizes.board_xywh.y + sizes.board_xywh.h * (tile::Coord::from(txy.y) as DrawLength / tile::Y::COUNT as DrawLength),
    }
}

#[cfg(feature = "debug-viz")]
fn tile_edge_to_tile_center(sizes: &Sizes, xy: DrawXY) -> DrawXY {
    DrawXY {
        x: xy.x + sizes.tile_side_length/2.,
        y: xy.y + sizes.tile_side_length/2.,
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
    Target(TradingSpot),
}

impl Default for AgentTarget {
    fn default() -> Self {
        Self::NoTarget
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Agent {
    inventory: Inventory,
    target: AgentTarget,
    previous_blocked_moves: [Option<tile::XY>; 4],
    blocked_forget_timer: u8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
        Self([(); MAX_NPCS_PER_CHUNK].map(|_| <_>::default()))
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

impl From<&Npc> for Speech {
    fn from(npc: &Npc) -> Self {
        match npc {
            Npc::Nobody
            | Npc::NoTrade
            // Later, we can have the agent make trades too.
            | Npc::Agent(_) => Self::Silence,
            Npc::Trade(trade) => Self::Trade(*trade),
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

fn none_adjacent_to(
    xys: &[tile::XY],
    center_xy: tile::XY
) -> bool {
    !xys.iter()
        .any(|&xy|
            center_xy.is_adjacent_to(xy)
        )
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
                                let xy = tile::i_to_xy(tile_i);
                                compile_time_assert!(NPC_ENTITY_MAX < Entity::MAX);
                                // We expect the maxmium value next_npc_index can
                                // take is `NPC_ENTITY_MAX + 1`.
                                if next_npc_index <= NPC_ENTITY_MAX
                                && npcs[next_npc_index] == Npc::Nobody
                                && none_adjacent_to(
                                    &xys.0[NPC_ENTITY_MIN as usize..next_npc_index as usize],
                                    xy
                                ) {
                                    xys[next_npc_index] = xy;

                                    next_npc_index = next_npc_index.saturating_add(1);
                                    pushed_npc_count += 1;
                                }
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

        let active_npcs = &mut npcs.0[NPC_ENTITY_MIN as usize..next_npc_index as usize];

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

/// A spot where you can stand talk to a trader.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct TradingSpot {
    xy: tile::XY,
    towards_trader: Dir,
}

const TRADING_SPOT_DECK_COUNT: usize = MAX_NPCS_PER_CHUNK * Dir::COUNT;
struct TradingSpotDeck {
    deck: [TradingSpot; TRADING_SPOT_DECK_COUNT],
    current_index: usize,
    max_index: usize,
}
compile_time_assert!(MAX_NPCS_PER_CHUNK > 0);

impl TradingSpotDeck {
    fn active_trading_spots(board: &mut Board) -> Option<TradingSpotDeck> {
        let mut deck = [<_>::default(); TRADING_SPOT_DECK_COUNT];

        let mut max_index = None;

        for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
            if let Npc::Trade(_) = board.npcs[entity] {
                for (xy, from_xy) in board.xys[entity].orthogonal_iter_with_dir() {
                    if board.is_walkable(xy) {
                        let i = max_index.map(|i| i + 1).unwrap_or(0);
                        deck[i] = TradingSpot {
                            xy,
                            towards_trader: from_xy.backward(),
                        };
                        max_index = Some(i);
                    }
                }
            }
        }

        max_index.map(|max_index| {
            xs_shuffle(&mut board.rng, &mut deck[0..=max_index]);

            TradingSpotDeck {
                deck,
                max_index,
                current_index: max_index,
            }
        })
    }

    fn draw(&mut self, rng: &mut Xs) -> TradingSpot {
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
    Direction(Dir),
    Interact,
}

fn dir_from_flags(flags: InputFlags) -> Option<Dir> {
    use Dir::*;
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


impl Input {
    fn from_flags(flags: InputFlags) -> Self {
        use Input::*;
        if INPUT_INTERACT_PRESSED & flags != 0 {
            Interact
        } else if let Some(dir) = dir_from_flags(flags) {
            Direction(dir)
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
        Direction(dir) => {
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
            if let Some(dir) = dir_from_flags(input_flags) {
                let mut target = player_xy!(state.board);
                move_xy(&mut target, dir, DIRECT_MOVE_VARIANT);

                for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
                    if state.board.xys[entity] == target {
                        let target_speech = (&state.board.npcs[entity]).into();

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

    if let Some(mut trader_spot_deck) = TradingSpotDeck::active_trading_spots(
        &mut state.board,
    ) {
        // TODO is it worth it to make this a per-frame thing? Or maybe store it
        // across frames and update it?
        let mut is_walkable_map: tile::IsWalkableMap = [false; TILES_LENGTH];
        for (i, element) in is_walkable_map.iter_mut().enumerate() {
            *element = state.board.tiles.tiles[i].kind.is_walkable();
        }
        let mut is_walkable_map_non_agents_only: tile::IsWalkableMap = is_walkable_map;
        for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
            if !state.board.npcs[entity].is_walkable() {
                let i = tile::xy_to_i(state.board.xys[entity]);
                is_walkable_map[i] = false;

                if let Npc::Agent(_) = state.board.npcs[entity] {
                    is_walkable_map_non_agents_only[i] = true;
                } else {
                    is_walkable_map_non_agents_only[i] = false;
                }
            }

            if let Npc::Agent(ref agent) = state.board.npcs[entity] {
                for xy_opt in &agent.previous_blocked_moves {
                    if let Some(xy) = xy_opt {
                        let i = tile::xy_to_i(*xy);
                        is_walkable_map[i] = false;
                    } else {
                        break;
                    }
                }
            }
        }
        is_walkable_map[tile::xy_to_i(state.board.xys[PLAYER_ENTITY])] = false;

        let mut move_pairs = Vec::with_capacity((NPC_ENTITY_MAX - NPC_ENTITY_MIN) as usize);

        macro_rules! fill_move_pairs {
            () => {
                for entity in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
                    if let Npc::Agent(ref mut agent) = state.board.npcs[entity] {
                        use AgentTarget::*;
                        match agent.target {
                            NoTarget => {
                                let spot = trader_spot_deck.draw(&mut state.board.rng);

                                // It is, of course, possible that the location will not be
                                // walkable by the time we get there. A form of the TOCTOU
                                // problem. The agent will need to deal with this when they
                                // get closer to there. Similarly they will also need to
                                // deal with the possibility of multiple agents having the
                                // same target, or the trader not being active at that time.
                                agent.target = Target(
                                    spot,
                                );
                            },
                            Target(TradingSpot { xy: target, ..}) => {
                                let goal = tile::WalkGoal {
                                    at: state.board.xys[entity],
                                    target,
                                };
                                let next = tile::next_walk_step(
                                    &is_walkable_map,
                                    goal
                                );

                                move_pairs.push((entity, next));
                            }
                        }
                    }
                }
            }
        }

        fill_move_pairs!();

        use std::collections::HashMap;
        let mut counts: HashMap<tile::XY, _> = HashMap::with_capacity(move_pairs.len());
        macro_rules! count {
            () => {
                for &(_entity, xy) in &move_pairs {
                    let counter = counts.entry(xy).or_insert(0);
                    *counter += 1;
                }
            }
        }

        count!();

        let mut needs_second_pass = false;
        for (_entity, xy) in &move_pairs {
            // We can use [] since we just inserted every xy.
            debug_assert_ne!(counts[xy], 0);
            let could_move = counts[xy] == 1;

            if !could_move {
                // If multiple agents are trying to enter a space at the same
                // time, then consider that.
                is_walkable_map[tile::xy_to_i(*xy)] = false;
                needs_second_pass = true;
                dbg!();
            }
        }

        for (entity, xy) in &move_pairs {
            if let Npc::Agent(ref mut agent) = state.board.npcs[*entity] {
                use AgentTarget::*;
                match agent.target {
                    NoTarget => {},
                    Target(TradingSpot { xy: target, towards_trader: _}) => {
                        let goal = tile::WalkGoal {
                            at: state.board.xys[*entity],
                            target,
                        };
                        let next_tiles_only = tile::next_walk_step(
                            &is_walkable_map_non_agents_only,
                            goal
                        );

                        if next_tiles_only != *xy {
                            for i in 0..agent.previous_blocked_moves.len() {
                                if agent.previous_blocked_moves[i].is_none() {
                                    agent.previous_blocked_moves[i] = Some(*xy);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        if needs_second_pass {
            // The is_walkable_map has changed, which means the
            // move_pairs and the counts need to be recalculated.
            move_pairs.clear();
            counts.clear();

            fill_move_pairs!();
            count!();
        }

        // Do the final moving
        for (entity, target_xy) in move_pairs {
            if let Npc::Agent(ref mut agent) = state.board.npcs[entity] {
                // We can use [] since we just inserted every xy.
                if counts[&target_xy] == 1 {
                    state.board.xys[entity] = target_xy;

                    // We need to drain these out eventually but we also need them
                    // to hang around for a bit to be effective.
                    agent.previous_blocked_moves =
                        agent.previous_blocked_moves.map(|op| {
                            op.and_then(|xy| {
                                let dist = tile::manhattan_distance(xy, target_xy);
                                compile_time_assert!(tile::Distance::MAX > 8);
                                (dist < 8).then(|| xy)
                            })
                        });
                    agent.previous_blocked_moves.sort_unstable_by_key(
                        // `true > false`, so `is_none` puts the `Some`s first.
                        |op| op.is_none()
                    );

                    use AgentTarget::*;
                    match agent.target {
                        Target(TradingSpot{xy, ..}) if xy == state.board.xys[entity] => {
                            // We have arrived.
                        }
                        Target(..) => {
                            // Wait until we arrive
                        }
                        _ => { debug_assert!(false, "unexpect AgentTarget") }
                    }
                }
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
        match &state.board.npcs[i as usize] {
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
                let draw_xy = draw_xy_from_tile(&state.sizes, xy);
                commands.push(Sprite(SpriteSpec{
                    sprite: SpriteKind::Eye(
                        EyeVariant::Agent,
                        state.board.eye_states[i].sprite()
                    ),
                    xy: draw_xy,
                }));

                use AgentTarget::*;
                match agent.target {
                    NoTarget => {},
                    Target(TradingSpot { xy: target, towards_trader}) => {
                        let target_draw_xy = draw_xy_from_tile(
                            &state.sizes,
                            target
                        );

                        commands.push(Sprite(SpriteSpec{
                            sprite: SpriteKind::Arrow(
                                towards_trader,
                                if target == xy {
                                    ArrowKind::Green
                                } else {
                                    ArrowKind::Red
                                }
                            ),
                            xy: target_draw_xy,
                        }));

                        #[cfg(feature = "debug-viz")]
                        commands.push(Line(LineSpec{
                            start: tile_edge_to_tile_center(&state.sizes, draw_xy),
                            end: tile_edge_to_tile_center(&state.sizes, target_draw_xy),
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
                sprite: SpriteKind::Arrow(Dir::Right, ArrowKind::Green),
                xy: DrawXY { x, y: top_edge_y },
            }));

            x += MARGIN + state.sizes.tile_side_length;

            commands.push(Sprite(SpriteSpec{
                sprite: SpriteKind::Item(trade.offer),
                xy: DrawXY { x, y: top_edge_y },
            }));
        },
    }

    #[cfg(feature = "debug-viz")]
    {
        let left_text_x = state.sizes.play_xywh.x + MARGIN;

        let small_section_h = state.sizes.draw_wh.h / 8. - MARGIN;

        #[allow(unused_mut)]
        let mut y = MARGIN;
        {
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

            #[cfg(any())]
            {
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

            #[cfg(any())]
            {
                y += small_section_h;

                let mut agent_text = String::with_capacity(256);

                for i in NPC_ENTITY_MIN..=NPC_ENTITY_MAX {
                    match &state.board.npcs[i as usize] {
                        Npc::Nobody => break,
                        Npc::Trade(_)
                        | Npc::NoTrade => {

                        },
                        Npc::Agent(agent) => {
                            agent_text.push_str(
                                &format!("{:?}\n", agent.previous_blocked_moves)
                            );
                        },
                    }
                }

                commands.push(Text(TextSpec{
                    text: agent_text,
                    xy: DrawXY { x: left_text_x, y },
                    wh: DrawWH {
                        w: state.sizes.play_xywh.w,
                        h: state.sizes.play_xywh.h - y
                    },
                    kind: TextKind::UI,
                }));
            }
        }
    }

    state.animation_timer += 1;
    if state.animation_timer >= ANIMATION_TIMER_LENGTH {
        state.animation_timer = 0;
    }

    state.board.regen.timer += 1;
    if state.board.regen.timer >= REGEN_TIMER_LENGTH {
        state.board.regen.timer = 0;
    }
}
