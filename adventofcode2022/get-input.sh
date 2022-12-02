set -e

session_cookie="$SESSION_COOKIE"

day="$1"
outdir="$2"

if [ -z "$day" ]; then
    echo "please specify day number"
    exit 1
fi
if [ -z "$outdir" ]; then
    echo "please specify outdir"
    exit 1
fi

outfile=$outdir/d`printf "%02d" $day`.txt
url=https://adventofcode.com/2021/day/$day/input


echo "retrieving: $url"
echo "saving to: $outfile"
curl -s \
    -L $url \
    -b "session=$session_cookie" \
    -o $outfile

wc $outfile
echo "OK"
