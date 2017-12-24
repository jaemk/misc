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
jnz g -13  --------->           |
jnz f 2                         |
sub h -1                        |
set g b                         |
sub g c                         |
jnz g 2  ------> blocking jump  |
jnz 1 3        |                |
sub b -17  <----                |
jnz 1 -23   -------------------->
*/

#[allow(unused_parens)]
#[allow(unused_variables)]
#[allow(unused_mut)]
#[allow(unused_assignments)]
pub fn _run_exact() -> i64 {
    let mut a = 1;
    let mut b = 84;
    let mut c = b;
    b *= 100;
    b -= -100_000;
    c -= -17_000;

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
                e -= -1;
                g = e;
                g -= b;
                if g == 0 { break }
            }
            d -= -1;
            g = d;
            g -= b;
            if g == 0 { break }
        }
        if f == 0 {
            h -= -1;
        }
        g = b;
        g -= c;
        if g == 0 { break; }
        b -= -17;
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
    // let mut b = (84 * 100) - (-100_000);
    // let c = b - (-17_000);
    let mut b = 108400;
    let c = 125400;

    let mut d = 0;
    let mut e = 0;
    let mut f = 0;
    let mut g = 0;
    let mut h = 0;

    loop {
        f = 1; // flag

        d = 2;
        loop {
            e = 2;
            loop {
                // g = d;
                // g *= e;
                // g -= b;
                g = (d * e) - b;
                if g == 0 {
                    f = 0;
                }
                e -= -1;

                // g = e;
                // g -= b;
                g = e - b;
                if g == 0 { break }
            }
            d -= -1;

            // g = d;
            // g -= b;
            g = d - b;
            if g == 0 { break }
        }
        if f == 0 {
            h -= -1;
        }

        // g = b;
        // g -= c;
        g = b - c;

        // main loop until b == 125400
        // --> so: (125400 - 108400) / 17 = 1000
        //     This section executes 1000 times and
        //     h is only incremented if f == 0.
        //     f is 0 when any (d * e) == b,
        //     so when b is not prime
        if g == 0 { break; }
        b -= -17;
    }
    h
}


struct RangeStep(usize, usize, usize);
impl Iterator for RangeStep {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        if self.0 < self.1 {
            let v = self.0;
            self.0 = self.0 + self.2;
            Some(v)
        } else {
            None
        }
    }
}


pub fn _run_minimal() -> i64 {
    let b_start = 108400;
    let c = 125400;
    let mut h = 0;
    'b: for b in RangeStep(b_start, c+1, 17) {
        for d in 2..b {
            for e in 2..b {
                if d * e == b {
                    h += 1;
                    continue 'b
                }
            }
        }
    }
    h
}


pub fn run_opt() -> i64 {
    let b = 108400;
    let c = 125400;
    RangeStep(b, c+1, 17).map(|b| {
        for d in 2..b {
            if b % d == 0 { return 1 }
        }
        0
    }).sum()
}

