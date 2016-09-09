
fn move_pieces(moves: &mut Vec<(u8, u8)>, n: u32, from: &mut Vec<u32>, to: &mut Vec<u32>, mid: &mut Vec<u32>) {
    if n > 0 {
        move_pieces(moves, n-1, from, mid, to);
        to.push(from.pop().unwrap());
        println!("{:?}\n{:?}\n{:?}\n---------", from, mid, to);
        move_pieces(moves, n-1, mid, to, from);
    }
}


pub fn hanoi(num_discs: u32) -> Vec<(u8, u8)> {
    let mut a = (1..num_discs+1).rev().collect::<Vec<u32>>();
    let mut b = Vec::with_capacity(num_discs as usize);
    let mut c = Vec::with_capacity(num_discs as usize);
    let mut moves = Vec::with_capacity(2usize.pow(num_discs)-1);
    move_pieces(&mut moves, num_discs, &mut a, &mut c, &mut b);
    moves
}
