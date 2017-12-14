use std::ops;


pub struct Ring {
    inner: Vec<usize>,
}
impl Ring {
    pub fn with_size(n: usize) -> Self {
        Self {
            inner: (0..n).collect::<Vec<_>>(),
        }
    }

    pub fn get_range(&self, mut start: usize, size: usize) -> Vec<usize> {
        let len = self.inner.len();
        assert!(size <= len);
        while start >= len { start -= len; }
        if (start + size) >= len {
            let mut v = self.inner[start..].to_vec();
            let n_from_front = (start + size) - len ;
            v.extend_from_slice(&self.inner[..n_from_front]);
            v
        } else {
            self.inner[start..start+size].to_vec()
        }
    }

    pub fn set_range(&mut self, start: usize, buf: &[usize]) {
        let inner_len = self.inner.len();
        assert!(buf.len() <= inner_len);
        for (i, val) in buf.iter().enumerate() {
            let mut ind = start + i;
            while ind >= inner_len { ind -= inner_len; }
            self.inner[ind] = *val;
        }
    }
}
impl ops::Deref for Ring {
    type Target = Vec<usize>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}


pub struct RingHasher<'a> {
    ind: usize,
    skip: usize,
    lengths: &'a [u8],
}
impl<'a> RingHasher<'a> {
    pub fn new(lengths: &'a [u8]) -> Self {
        Self {
            ind: 0,
            skip:0,
            lengths
        }
    }

    pub fn hash_step(&mut self, ring: &mut Ring) {
        for len in self.lengths {
            let len = *len as usize;
            let mut slice = ring.get_range(self.ind, len);
            slice.reverse();
            ring.set_range(self.ind, &slice);
            self.ind += len + self.skip;
            self.skip += 1;
        }
    }

    pub fn knot_hash(&mut self, ring: &mut Ring) -> Vec<u8> {
        for _ in 0..64 {
            self.hash_step(ring);
        }
        ring.chunks(16)
            .map(|chunk| chunk.iter().fold(0, |acc, n| acc ^ n) as u8)
            .collect::<Vec<u8>>()
    }
}


pub mod knot {
    use super::*;

    pub fn hash(input: &str) -> Vec<u8> {
        let mut lengths = input.trim().as_bytes().to_vec();
        lengths.extend_from_slice(&[17, 31, 73, 47, 23]);

        let mut ring = Ring::with_size(256);
        let mut hasher = RingHasher::new(&lengths);
        hasher.knot_hash(&mut ring)
    }
}

