use {Dir};


pub struct BitTape {
    cursor: usize,
    inner: Vec<bool>,
    growth_factor: usize,
}
impl BitTape {
    pub fn with_size(size: usize) -> Self {
        Self {
            inner: vec![false; size],
            cursor: size / 2,
            growth_factor: 2,
        }
    }

    fn expand(&mut self) {
        let size = self.inner.len();
        let new_size = size * self.growth_factor;
        let padding = new_size - size;
        let pad_left = padding / 2;
        let pad_right = padding - pad_left;
        let mut new_tape = Vec::with_capacity(new_size);
        for _ in 0..pad_left {
            new_tape.push(false);
        }
        new_tape.extend_from_slice(&self.inner);
        for _ in 0..pad_right {
            new_tape.push(false);
        }
        self.cursor += pad_left;
        self.inner = new_tape;
    }

    pub fn step(&mut self, dir: &Dir) {
        match *dir {
            Dir::Left => {
                if self.cursor == 0 {
                    self.expand();
                }
                self.cursor -= 1;
            }
            Dir::Right => {
                if self.cursor == self.inner.len() - 1 {
                    self.expand();
                }
                self.cursor += 1;
            }
        }
    }

    pub fn set(&mut self, bit: bool) {
        self.inner[self.cursor] = bit;
    }

    pub fn get(&self) -> bool {
        self.inner[self.cursor]
    }

    pub fn count_ones(&self) -> usize {
        self.inner.iter().map(|&b| if b { 1 } else { 0 }).sum()
    }
}


#[cfg(test)]
mod test{
    use super::*;

    #[test]
    fn tape_grows_even_left() {
        let mut tape = BitTape::with_size(4);
        assert_eq!(tape.inner.len(), 4);
        assert_eq!(tape.cursor, 2);
        tape.step(&Dir::Left);
        assert_eq!(tape.cursor, 1);
        tape.step(&Dir::Left);
        tape.set(true);
        assert_eq!(tape.cursor, 0);
        tape.step(&Dir::Left);
        assert_eq!(tape.inner.len(), 8);
        assert_eq!(tape.cursor, 1);
        tape.step(&Dir::Right);
        assert_eq!(tape.get(), true);
    }

    #[test]
    fn tape_grows_even_right() {
        let mut tape = BitTape::with_size(4);
        assert_eq!(tape.inner.len(), 4);
        assert_eq!(tape.cursor, 2);
        tape.step(&Dir::Right);
        tape.set(true);
        assert_eq!(tape.cursor, 3);
        tape.step(&Dir::Right);
        assert_eq!(tape.inner.len(), 8);
        assert_eq!(tape.cursor, 6);
        tape.step(&Dir::Left);
        assert_eq!(tape.get(), true);
    }

    #[test]
    fn tape_grows_odd_left() {
        let mut tape = BitTape::with_size(5);
        assert_eq!(tape.inner.len(), 5);
        assert_eq!(tape.cursor, 2);
        tape.step(&Dir::Left);
        assert_eq!(tape.cursor, 1);
        tape.step(&Dir::Left);
        tape.set(true);
        assert_eq!(tape.cursor, 0);
        tape.step(&Dir::Left);
        assert_eq!(tape.inner.len(), 10);
        assert_eq!(tape.cursor, 1);
        tape.step(&Dir::Right);
        assert_eq!(tape.get(), true);
    }

    #[test]
    fn tape_grows_odd_right() {
        let mut tape = BitTape::with_size(5);
        assert_eq!(tape.inner.len(), 5);
        assert_eq!(tape.cursor, 2);
        tape.step(&Dir::Right);
        assert_eq!(tape.cursor, 3);
        tape.step(&Dir::Right);
        tape.set(true);
        assert_eq!(tape.cursor, 4);
        tape.step(&Dir::Right);
        assert_eq!(tape.inner.len(), 10);
        assert_eq!(tape.cursor, 7);
        tape.step(&Dir::Left);
        assert_eq!(tape.get(), true);
    }
}

