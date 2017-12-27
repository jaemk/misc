
extern crate refact;

#[test]
fn working() {
    assert_eq!(0, 0);
}

#[test]
fn revfact() {
    assert_eq!(refact::refact(120), Some(5));
    assert_eq!(refact::refact(150), None);
    assert_eq!(refact::refact(3628800), Some(10));
}
