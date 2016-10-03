# [wandering finders](https://www.reddit.com/r/dailyprogrammer/comments/53ijnb/20160919_challenge_284_easy_wandering_fingers/)

Running rust benchmarks with `cargo bench` requires nightly.

Otherwise:
```
cargo build --release
time ./target/release/swipes

qwertyuytresdftyuioknn: Some(["queen", "question"])
gijakjthoijerjidsdfnokg: Some(["gaeing", "garring", "gathering", "gating", "geeing", "gieing", "going", "goring"])

real    0m0.046s
user    0m0.044s
sys     0m0.000s
```

