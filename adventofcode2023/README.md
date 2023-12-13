# advent of code 2023

`cargo run` from within the rust directory will automatically
pull any missing input files. To manually pull input files,
use `get-input.sh`.

Both methods require a `SESSION_COOKIE` env var to be set.

* Create a `.env.local` file with:
```bash
# set to your user's session cookie
SESSION_COOKIE=xxxxx 
```
* `source .env`
* `get-input.sh $day input/`
