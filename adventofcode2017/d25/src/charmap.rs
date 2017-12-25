
pub struct AsciiLowerMap<T> {
    inner: Vec<Option<T>>
}
impl<T> AsciiLowerMap<T> {
    pub fn new() -> Self {
        let inner = (0..26).map(|_| None).collect::<Vec<_>>();
        Self { inner }
    }

    fn as_index(c: char) -> usize {
        ((c as u8) - b'a') as usize
    }

    pub fn get(&self, c: char) -> Option<&T> {
        self.inner[Self::as_index(c)].as_ref()
    }

    pub fn set(&mut self, c: char, b: T) {
        self.inner[Self::as_index(c)] = Some(b);
    }
}

