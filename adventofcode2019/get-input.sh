set -e

session_cookie="$SESSION_COOKIE"

day="$1"
outfile=./input/d`printf "%02d" $day`.txt
url=https://adventofcode.com/2019/day/$day/input

if [ -z "$day" ]; then
    echo "please specify day number"
    exit 1
fi

echo "retrieving: $url"
echo "saving to: $outfile"
curl -s \
    -L $url \
    -b "session=$session_cookie" \
    -o $outfile

wc $outfile
echo "OK"
