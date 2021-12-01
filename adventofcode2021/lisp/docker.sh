#!/bin/bash

set -exuo pipefail

cmd="${1:-build}"
version="$(git rev-parse HEAD | awk '{ printf "%s", substr($0, 0, 7) }')"

# options
reg="${REGISTRY:-docker.jaemk.me}"
app="advent"
port_map_port="${PORT_MAP_PORT:-3003}"
port_map="${PORT_MAP:-127.0.0.1:$port_map_port:$port_map_port}"

env_file=""
if [ -f .env.docker ]; then
    env_file="--env-file .env.docker"
fi

if [ -z "$cmd" ]; then
    echo "missing command..."
    exit 1
elif [ "$cmd" = "build" ]; then
    cp -r ../input input
    if [ ! -z "$version" ]; then
        docker build -t $reg/$app:$version .
    fi
    docker build -t $reg/$app:latest .
    rm -r input/
elif [ "$cmd" = "push" ]; then
    $0 build
    docker push $reg/$app:$version
    docker push $reg/$app:latest
elif [ "$cmd" = "run" ]; then
    $0 build
    docker run --rm -it --init -p $port_map $env_file $reg/$app:latest
elif [ "$cmd" = "shell" ]; then
    $0 build
    docker run --rm -it --init -p $port_map $env_file $reg/$app:latest /bin/bash
fi
