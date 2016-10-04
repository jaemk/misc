extern crate swipes;

#[test]
fn working() {
    assert_eq!(0, 0);
}

#[test]
fn fuzzy_finds_hash() {
    let mut m = swipes::MatcherHash::new();
    m.load("enable1.txt");
    assert_eq!(m.contained_by("qwertyuytresdftyuioknn").unwrap(),
               vec!["queen", "question"]);
    assert_eq!(m.contained_by("gijakjthoijerjidsdfnokg").unwrap(),
               vec!["gaeing", "garring", "gathering", "gating",
                    "geeing", "gieing", "going", "goring"]);
}

#[test]
fn fuzzy_finds_vec() {
    let mut m = swipes::MatcherVec::new();
    m.load("enable1.txt");
    assert_eq!(m.contained_by("qwertyuytresdftyuioknn").unwrap(),
               vec!["queen", "question"]);
    assert_eq!(m.contained_by("gijakjthoijerjidsdfnokg").unwrap(),
               vec!["gaeing", "garring", "gathering", "gating",
                    "geeing", "gieing", "going", "goring"]);
}

#[test]
fn fuzzy_raw() {
    assert_eq!(swipes::contained_by_raw("enable1.txt", "qwertyuytresdftyuioknn").unwrap(),
               vec!["queen", "question"]);
    assert_eq!(swipes::contained_by_raw("enable1.txt", "gijakjthoijerjidsdfnokg").unwrap(),
               vec!["gaeing", "garring", "gathering", "gating",
                    "geeing", "gieing", "going", "goring"]);
}

