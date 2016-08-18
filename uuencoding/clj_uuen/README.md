# uuencoding/decoding

[uuencoding](https://www.reddit.com/r/dailyprogrammer/comments/4xy6i1/20160816_challenge_279_easy_uuencoding/)

`lein run`/ `lein test`

In the repl,
```(->> "this is neat"
        (uuencode :filename "misc" :content)    ;; encode
        (decode))                               ;;=> "this is neat"
```

