extern crate encode;

#[test]
fn working() {
    assert_eq!(0, 0);
}

#[test]
fn unsigned_byte_input() {
    // takes string input, converts to maxbyte-arrays, output formatted string.
    // preserve whatever line grouping there was in the input.
    let input = "12\n255\n256\n510\n512 44 1024";
    let out_str = "12\n255 0\n255 1\n255 255 0\n255 255 2 44 255 255 255 255 4";
    let out_arr = vec![
        vec![vec![12]], vec![vec![255, 0]], vec![vec![255, 1]], vec![vec![255, 255, 0]],
        vec![vec![255, 255, 2], vec![44], vec![255, 255, 255, 255, 4]],
    ];
    let enc = encode::encode_maxbyte_array(input);
    assert_eq!(enc, out_arr);
    assert_eq!(encode::maxbyte_array_to_string(enc), out_str);
}
