#[derive(Debug)]
struct Tower{
    hold: Vec<u32>,
    num: u8,
}

impl Tower {
    fn new(hold: Vec<u32>, num: u8) -> Tower {
        Tower{hold: hold, num: num}
    }
    fn pop(&mut self) -> u32 {
        self.hold.pop().unwrap()
    }
    fn push(&mut self, item: u32) {
        self.hold.push(item);
    }
}

fn move_pieces(moves: &mut Vec<(u8, u8)>, n: u32,
               from: &mut Tower, to: &mut Tower,
               mid: &mut Tower) {
    if n > 0 {
        move_pieces(moves, n-1, from, mid, to);
        to.push(from.pop());
        moves.push((from.num, to.num));
        println!("moved from {:?} to {:?}", from.num, to.num);
        println!("{:?}\n{:?}\n{:?}\n---------", from, mid, to);
        move_pieces(moves, n-1, mid, to, from);
    }
}


pub fn hanoi(num_discs: u32) -> Vec<(u8, u8)> {
    let mut a = Tower::new((1..num_discs+1).rev().collect::<Vec<u32>>(), 1u8);
    let mut b = Tower::new(Vec::with_capacity(num_discs as usize), 2u8);
    let mut c = Tower::new(Vec::with_capacity(num_discs as usize), 3u8);
    let mut moves = Vec::with_capacity(2usize.pow(num_discs)-1);
    move_pieces(&mut moves, num_discs, &mut a, &mut c, &mut b);
    moves
}
