/*!
Given a string and a number of lines k, print the string in zigzag form.
In zigzag, characters are printed out diagonally from top left to bottom
right until reaching the kth line, then back up to top right, and so on.

For example, given the sentence "thisisazigzag" and k = 4, you should print:
```
t     a     g
 h   s z   a
  i i   i z
   s     g
``
*/
fn zigzag(input: &str, n: usize) -> String {
    if n <= 1 {
        return input.to_string();
    }
    let mut lines = (0..n).map(|_| vec![]).collect::<Vec<_>>();

    let mut inc = true;
    let mut cur_line_no = 0;
    for c in input.chars() {
        for (line_no, line) in lines.iter_mut().enumerate() {
            if line_no == cur_line_no {
                line.push(c);
            } else {
                line.push(' ');
            }
        }
        if cur_line_no == n - 1 {
            // reached the bottom
            inc = false
        } else if cur_line_no == 0 {
            // reached the top
            inc = true
        }

        if inc {
            // move down
            cur_line_no += 1;
        } else {
            // move up
            cur_line_no -= 1;
        }
    }
    lines
        .into_iter()
        .map(|line| line.into_iter().collect::<String>() + "\n")
        .collect::<String>()
        .trim()
        .to_string()
}

fn main() {
    let input = "thisisazigzagwoohoo";
    let s = zigzag(input, 4);
    println!("{}", s);
}
