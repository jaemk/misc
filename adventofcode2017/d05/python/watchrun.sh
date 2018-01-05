#!/bin/bash

if [[ -z $1 ]]; then
    echo "usage: $0 <file.py>"
    exit 1
fi

set +ex

watchexec -c -e py "\
    echo '* Checking...' \
    && python3.6 -m mypy --strict --incremental --follow-imports=skip ${1} \
    && echo '* Testing...' \
    && ./${1} test \
    && echo '* Running...' \
    && ./${1}"

