pub fn refact(fac: &i32) -> Option<i32> {
    let mut div: i32 = 2;
    let mut val: i32 = *fac;
    while val % div == 0 {
        val = val / div;
        div += 1;
    }
    if val == 1 {
        Some(div - 1)
    } else {
        None
    }
}
pub fn reverse_factorial(n: &i32) -> String {
    let val = match refact(n) {
        Some(v) => format!(" = {}!", v),
        None => "   NONE".to_string(),
    };
    format!("{}{}", n, val)
}
