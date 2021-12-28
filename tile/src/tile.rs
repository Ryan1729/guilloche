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

use xs::{Xs, xs_u32, counted_enum_def, from_rng_enum_def};
use std::collections::HashMap;

pub type Count = u32;

pub type Coord = u8;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct X(Coord);

impl X {
    pub const MAX: Self = Self(0b0011_1111);
    pub const COUNT: Count = (X::MAX.0 as Count) + 1;

    pub fn from_rng(rng: &mut Xs) -> Self {
        Self(xs_u32(rng, 0, Self::COUNT) as Coord)
    }

    pub fn saturating_add_one(&self) -> Self {
        Self(core::cmp::min(self.0.saturating_add(1), Self::MAX.0))
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
    pub const MAX: Self = Self(0b0011_1111);
    pub const COUNT: Count = (Y::MAX.0 as Count) + 1;

    pub fn from_rng(rng: &mut Xs) -> Self {
        Self(xs_u32(rng, 0, Self::COUNT) as Coord)
    }

    pub fn saturating_add_one(&self) -> Self {
        Self(core::cmp::min(self.0.saturating_add(1), Self::MAX.0))
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

impl Dir {
    /// The `Dir` 180 degrees from this one.
    pub fn backward(self) -> Self {
        use Dir::*;
        match self {
            Up => Down,
            UpRight => DownLeft,
            Right => Left,
            DownRight => UpLeft,
            Down => Up,
            DownLeft => UpRight,
            Left => Right,
            UpLeft => DownRight,
        }
    }
}

pub type Distance = Count;

pub const TILES_WIDTH: usize = X::COUNT as _;
pub const TILES_HEIGHT: usize = Y::COUNT as _;
pub const TILES_LENGTH: usize = XY::COUNT as _;

pub fn manhattan_distance(
    XY{ x: x1, y: y1 }: XY,
    XY{ x: x2, y: y2 }: XY
) -> Distance {
    compile_time_assert!(i16::BITS > Coord::BITS);
    compile_time_assert!(Distance::BITS >= i16::BITS);
    macro_rules! to { ($c: ident) => {{ Coord::from($c) as i16 }} }
    ((to!(x1) - to!(x2)).abs() + (to!(y1) - to!(y2)).abs()) as Distance
}

pub type IsWalkableMap = [bool; TILES_LENGTH];

fn walkable_from_iter(is_walkable_map: &IsWalkableMap, at: XY) -> impl Iterator<Item = XY> + '_ {
    at.orthogonal_iter().filter(|&xy| {
        let i = xy_to_i(xy);

        is_walkable_map[i]
    })
}

pub struct WalkGoal {
    pub at: XY,
    pub target: XY,
}

pub fn next_walk_step(
    is_walkable_map: &IsWalkableMap,
    WalkGoal{at, target}: WalkGoal
) -> XY {
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
    let mut came_from: HashMap<XY, XY>
        = HashMap::with_capacity(LONGEST_PATH_LENGTH);

    // For a given xy, g_score[xy] is the cost of the cheapest path from at to xy
    // currently known.
    let mut g_score: HashMap<XY, Distance>
        = HashMap::with_capacity(LONGEST_PATH_LENGTH);
    g_score.insert(at, 0);

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    struct ScoredXY {
        // For a ScoredXY with a given xy, we mantain that
        // f_score[xy] := g_score[xy] + manhattan_distance(at, xy).
        // f_score[xy] represents our current best guess as to how short a path from
        // at to target can be if it goes through xy.
        f_score: Distance,
        xy: XY,
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

const ORTHOGONAL_COUNT: usize = 4; // We're in 2D.

struct XYOrthogonalIterWithDir {
    pairs: [Option<(XY, Dir)>; ORTHOGONAL_COUNT],
    index: usize
}

impl XYOrthogonalIterWithDir {
    #[allow(unused_assignments)]
    fn new(xy: XY) -> Self {
        let mut pairs = [None; ORTHOGONAL_COUNT];
        compile_time_assert!(ORTHOGONAL_COUNT < usize::MAX);

        let mut i = 0;
        macro_rules! push {
            ($method: ident, $dir: ident) => {
                let mut new_xy = xy;
                new_xy.$method();
                if new_xy != xy {
                    pairs[i] = Some((new_xy, Dir::$dir));
                }
                // This is unused in the last macro invocation.
                i += 1;
            }
        }

        push!(move_up, Up);
        push!(move_left, Left);
        push!(move_right, Right);
        push!(move_down, Down);

        XYOrthogonalIterWithDir {
            pairs,
            index: 0,
        }
    }
}

impl Iterator for XYOrthogonalIterWithDir {
    type Item = (XY, Dir);

    fn next(&mut self) -> Option<Self::Item> {
        let output = self.pairs.get(self.index).and_then(|&xy| xy);
        self.index += 1;
        output
    }
}

impl XY {
    pub fn orthogonal_iter_with_dir(self) -> impl Iterator<Item = (Self, Dir)> {
        XYOrthogonalIterWithDir::new(self)
    }

    pub fn orthogonal_iter(self) -> impl Iterator<Item = Self> {
        self.orthogonal_iter_with_dir()
            .map(|pair| pair.0)
    }
}