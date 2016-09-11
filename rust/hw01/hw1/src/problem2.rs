pub fn sieve(n: u32) -> Vec<u32> {
    let acc = (2..n+1).collect::<Vec<_>>();
    (2..n+1).fold(acc, |mut acc, item| {
        match acc.binary_search(&item) {
            Err(_) => acc,
            Ok(_) => {
                let mut np = item + item;
                while np <= n {
                    match acc.binary_search(&np) {
                        Ok(ind) => acc.remove(ind),
                        Err(_) => 0,
                    };
                    np = np + item;
                };
                acc
            }
        }
    })
}
