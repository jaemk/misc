extern crate swipes;

pub fn main() {
    println!("qwertyuytresdftyuioknn: {:?}",
             swipes::contained_by_raw("enable1.txt", "qwertyuytresdftyuioknn"));
    println!("gijakjthoijerjidsdfnokg: {:?}",
             swipes::contained_by_raw("enable1.txt", "gijakjthoijerjidsdfnokg"));
}
