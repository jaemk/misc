extern crate fibrep;

#[test]
fn tests_working() {
    assert_eq!(0, 0);
}

#[test]
fn test_basic() {
    let input = vec![4, 100, 30];
    assert_eq!(vec![vec![3, 1], vec![89, 8, 3], vec![21, 8, 1]],
               input.iter().map(|&n| { fibrep::into_fibs(n) }).collect::<Vec<_>>());
}

#[test]
fn test_challenge() {
    let input = vec![120, 34, 88, 90, 320];
    assert_eq!(
        vec![
            vec![89, 21, 8, 2], vec![34], vec![55, 21, 8, 3, 1],
            vec![89, 1], vec![233, 55, 21, 8, 3],
        ],
        input.iter().map(|&n| { fibrep::into_fibs(n) }).collect::<Vec<_>>());
}
