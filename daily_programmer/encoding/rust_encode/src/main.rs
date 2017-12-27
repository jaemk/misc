extern crate encode;


fn main() {
    let input = "12\n255\n256\n510\n512 44 1024";
    println!("{}", input);
    let enc = encode::encode_maxbyte_array(input);
    println!("{:?}", enc);
    let maxb_string = encode::maxbyte_array_to_string(enc);
    println!("{}", maxb_string);

    let dec = encode::decode_maxbyte_array(&maxb_string);
    println!("{:?}", dec);
    let dec_string = encode::maxbyte_array_to_string(dec);
    println!("{}", dec_string);
}

