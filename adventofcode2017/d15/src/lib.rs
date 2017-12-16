use std::sync;
use std::thread;


pub struct Generator {
    factor: u64,
    multiple_of: u64,
    div: u64,
    value: u64,
}
impl Generator {
    pub fn new(initial: u64, factor: u64, multiple_of: u64) -> Self {
        Self {
            factor,
            multiple_of,
            div: 2147483647,
            value: initial,
        }
    }
}
impl Iterator for Generator {
    type Item = u64;
    fn next(&mut self) -> Option<Self::Item> {
        let mut v = (self.factor * self.value) % self.div;
        let next = loop {
            if v % self.multiple_of == 0 { break v }
            v = (self.factor * v) % self.div;
        };
        self.value = next;
        Some(next)
    }
}


pub fn judge(a_start: u64, a_mult: u64, b_start: u64, b_mult: u64, iters: usize) -> u64 {
    let gen_a = Generator::new(a_start, 16807, a_mult);
    let gen_b = Generator::new(b_start, 48271, b_mult);
    gen_a.zip(gen_b)
        .take(iters)
        .fold(0, |sum, (a, b)| {
            if (0xFFFF & a) == (0xFFFF & b) {
                sum + 1
            } else {
                sum
            }
        })
}


pub fn part1(a_start: u64, b_start: u64) -> u64 {
    return judge(a_start, 1, b_start, 1, 40_000_000);
}


pub fn part2(a_start: u64, b_start: u64) -> u64 {
    return judge(a_start, 4, b_start, 8, 5_000_000);
}


// ------------------------------------------------------
// ------------------------------------------------------


pub fn judge_channels(buf_send_size: usize, a_start: u64, a_mult: u64, b_start: u64, b_mult: u64, iters: usize) -> u64 {
    let (send_a, recv_a) = sync::mpsc::channel();
    let (send_b, recv_b) = sync::mpsc::channel();
    assert!(iters % buf_send_size == 0, "`iters` must be a multiple of `buf_send_size`");
    let buf_size = 10000;
    let buf_iters = iters / buf_size;
    thread::spawn(move || {
        let gen_a = Generator::new(a_start, 16807, a_mult);
        let mut buf = Vec::with_capacity(buf_size);
        for val in gen_a.take(iters) {
            buf.push(val);
            if buf.len() < buf_size { continue }
            let mut swapped = Vec::with_capacity(buf_size);
            std::mem::swap(&mut buf, &mut swapped);
            send_a.send(swapped).expect("Failed sending A");
        }
    });
    thread::spawn(move || {
        let gen_b = Generator::new(b_start, 48271, b_mult);
        let mut buf = Vec::with_capacity(buf_size);
        for val in gen_b.take(iters) {
            buf.push(val);
            if buf.len() < buf_size { continue }
            let mut swapped = Vec::with_capacity(buf_size);
            std::mem::swap(&mut buf, &mut swapped);
            send_b.send(swapped).expect("Failed sending B");
        }
    });
    (0..buf_iters).fold(0, |sum, _| {
        let a_buf = recv_a.recv().expect("Failed receiving from A");
        let b_buf = recv_b.recv().expect("Failed receiving from B");
        a_buf.iter().zip(b_buf.iter()).fold(sum, |sum, (a, b)| {
            if (0xFFFF & a) == (0xFFFF & b) {
                sum + 1
            } else {
                sum
            }
        })
    })
}


pub fn part1_channels(buf_send_size: usize, a_start: u64, b_start: u64) -> u64 {
    return judge_channels(buf_send_size, a_start, 1, b_start, 1, 40_000_000);
}


pub fn part2_channels(buf_send_size: usize, a_start: u64, b_start: u64) -> u64 {
    return judge_channels(buf_send_size, a_start, 4, b_start, 8, 5_000_000);
}

