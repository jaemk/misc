extern crate crypto;

use crypto::md5::Md5;
use crypto::digest::Digest;


pub enum Part {
    One,
    Two,
}


/// Returns Ok if the index is a valid value (0-7), the location is empty ('-'),
/// and the item has been inserted.
fn insert_at(buf: &mut Vec<char>, index: char, item: char) -> Result<(), ()> {
    match index as u8 {
        48 ... 55 => { // chars '0' - '7'
            let i = index.to_digit(10).unwrap() as usize;
            if buf[i] != '-' { return Err(()) }
            buf[i] = item;
            Ok(())
        }
        _ => {
            Err(())
        }
    }
}


pub fn eval(door_id: &str, part: Part) -> String {
    println!("... checking a lot of md5s... may take a bit...");
    const TO_HEX: [char; 16] = [
        '0', '1', '2', '3', '4', '5',
        '6', '7', '8', '9', 'a', 'b',
        'c', 'd', 'e', 'f',
    ];
    let id_bytes = door_id.as_bytes();

    let mut hasher = Md5::new();
    let mut result = [0; 16];

    let mut password = vec!['-'; 8];
    let mut inserted = 0;

    for n in 0..std::u64::MAX {
        let suffix = n.to_string();
        let suffix = suffix.as_bytes();
        hasher.input(id_bytes);
        hasher.input(suffix);
        hasher.result(&mut result);

        // Check the first 5 chars of the hex-representation of the 16 byte result.
        // Hex uses two chars to represent each byte, so the first 5 chars would be
        // representing the first 2.5 bytes (20bits). These should all be 0.
        // The fifth char is the left 4 bits of the third byte,
        // the sixth is the right 4 bits of the third byte,
        // the seventh is the left 4 bits of the fourth byte.
        if result[0] == 0 && result[1] == 0 && result[2] >> 4 == 0 {
            let right_half_third_byte = result[2] & 15;
            let left_half_fourth_byte = result[3] >> 4;
            let sixth_char = TO_HEX[right_half_third_byte as usize];
            let seventh_char = TO_HEX[left_half_fourth_byte as usize];
            match part {
                Part::One => {
                    // collect in the order found
                    password[inserted] = sixth_char;
                    inserted += 1;
                }
                Part::Two => {
                    // insert in the position specified by the sixth_char
                    // ignoring repeats and invalid indices
                    if let Ok(_) = insert_at(&mut password, sixth_char, seventh_char) {
                        inserted += 1;
                    }
                }
            }
        }
        if inserted == 8 {
            return password.into_iter().collect::<String>()
        }
        hasher.reset()
    }
    "".to_string()
}
