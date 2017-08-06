#!/usr/bin/python
import time
import os

BASE_DIR = os.path.dirname(os.path.realpath(__file__))


VERS = [
    "rust_v1",
    "rust_v2",
]


def main():
    print("Compiling `VERS`: {}".format(VERS))
    for ver in VERS:
        path = os.path.join(BASE_DIR, ver)
        os.chdir(path)
        os.system('cargo build --release')

    print("\nBenching VERS:")
    for ver in VERS:
        print("** {} **".format(ver))
        path = os.path.join(BASE_DIR, ver)
        os.chdir(path)
        start = time.time()
        os.system('target/release/{} ../input.txt 1 2'.format(ver))
        elap = time.time() - start
        print("{}: {}s\n".format(ver, elap))


if __name__ == '__main__':
    main()
