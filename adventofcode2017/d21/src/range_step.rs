
pub struct RangeStep(pub usize, pub usize, pub usize);
impl Iterator for RangeStep {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        if self.0 < self.1 {
            let v = self.0;
            self.0 += self.2;
            Some(v)
        } else {
            None
        }
    }
}

