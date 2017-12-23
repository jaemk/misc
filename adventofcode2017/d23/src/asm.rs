/*
-- starting, a=1
set b 84      ------------------------
set c b                         |
jnz a 2     -- always -->       |   i
jnz 1 5                 |       |   n
mul b 100     <----------       |   i
sub b -100000                   |   t
set c b                         |
sub c -17000   -----------------------
set f 1  <-------- main loop start
set d 2                         |
set e 2  <-----------           |
set g d  <-------   |           |
mul g e         |   |           |
sub g b         |   |           |
jnz g 2  -->    |   |           |
set f 0    |    |   |           |
sub e -1 <--    |   |           |
set g e         |   |           |
sub g b         |   |           |
jnz g -8   ----->   |           |
sub d -1            |           |
set g d             |           |
sub g b             |           |
jnz g -13  ----------           |
jnz f 2                         |
sub h -1                        |
set g b                         |
sub g c                         |
jnz g 2  ------> mainblock      |
jnz 1 3        |                |
sub b -17  <----                |
jnz 1 -23   -------------------->
*/

#[allow(unused_parens)]
#[allow(unused_variables)]
#[allow(unused_mut)]
#[allow(unused_assignments)]
pub fn _run_exact() -> i64 {
    // init
    let mut a = 1;
    let mut b = (84 * 100) - 100_000;
    let c = (b - 17_000);

    let mut d = 0;
    let mut e = 0;
    let mut f = 0;
    let mut g = 0;
    let mut h = 0;

    loop {
        f = 1;
        d = 2;
        loop {
            e = 2;
            loop {
                g = d;
                g *= e;
                g -= b;
                if g == 0 {
                    f = 0;
                }
                e -= 1;
                g = e;
                g -= b;
                if g == 0 { break }
            }
            d -= 1;
            g = d;
            g -= b;
            if g == 0 { break }
        }
        if f == 0 {
            h -= 1;
        }
        g = b;
        g -= c;
        if g == 0 { break; }
        b -= 17;
    }
    h
}


#[allow(unused_parens)]
#[allow(unused_variables)]
#[allow(unused_mut)]
#[allow(unused_assignments)]
pub fn _run_annotated() -> i64 {
    // init
    // let mut a = 1;
    // let mut b = (84 * 100) - 100_000;
    // let c = (b - 17_000);
    let mut b = -91600;
    let c = -108600;

    let mut d = 0;
    let mut e = 0;
    let mut f = 0;
    let mut g = 0;
    let mut h = 0;

    loop {
        f = 1; // would be set to zero by the inner most loop
        d = 2; // d will equal -b, but it isn't used for anything
        loop {
            e = 2;
            loop {
                // g = d;
                // g *= e;
                // g -= b;
                //
                // e must == -45800
                g = (2 * e) - b;
                if g == 0 {
                    f = 0;
                }
                e -= 1;

                // e must == -91600
                g = e;
                g -= b;
                if g == 0 { break }
            }
            d -= 1;
            // g = d;
            // g -= b;
            g = d - (-91600);
            // d must == -91600
            if g == 0 { break }
        }
        //
        // --> f is always 0 here
        // if f == 0 {
        //     h -= 1;
        // }
        h -= 1;
        // g = b;
        // g -= c;
        g = (b - (-108600));
        // b must == -108600
        // --> so h = (108600 - 91600) / 17 = 1000
        //     and it's negative, -1000
        if g == 0 { break; }
        b -= 17;
    }
    h
}


pub fn run_minimal() -> i64 {
    let mut b = -91600;
    let mut h = 0;
    loop {
        h -= 1;
        let g = (b - (-108600));
        if g == 0 { break; }
        b -= 17;
    }
    h
}

