extern crate uuen;

pub fn main() {
    let input1 = "Cat";
    println!("Encoding `{}` ->\n{}", input1, uuen::encode(input1, None));
    println!("");
    let input2 = "I feel very strongly about you!";
    println!("Encoding `{}` ->\n{}", input2, uuen::encode(input2, None));
}
