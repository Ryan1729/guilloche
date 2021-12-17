pub type Seed = [u8; 16];

pub type Xs = [core::num::Wrapping<u32>; 4];

pub fn xorshift(xs: &mut Xs) -> u32 {
    let mut t = xs[3];

    xs[3] = xs[2];
    xs[2] = xs[1];
    xs[1] = xs[0];

    t ^= t << 11;
    t ^= t >> 8;
    xs[0] = t ^ xs[0] ^ (xs[0] >> 19);

    xs[0].0
}

pub fn xs_u32(xs: &mut Xs, min: u32, one_past_max: u32) -> u32 {
    (xorshift(xs) % (one_past_max - min)) + min
}

pub fn xs_shuffle<A>(rng: &mut Xs, slice: &mut [A]) {
    for i in 1..slice.len() as u32 {
        // This only shuffles the first u32::MAX_VALUE - 1 elements.
        let r = xs_u32(rng, 0, i + 1) as usize;
        let i = i as usize;
        slice.swap(i, r);
    }
}

pub fn new_seed(rng: &mut Xs) -> Seed {
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

pub fn xs_from_seed(mut seed: Seed) -> Xs {
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
