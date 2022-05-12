#!/usr/bin/env bash

# Use system-wide coq instead, not this one

zlib_path=$(pkg-config zlib --cflags)
zlib_path=${zlib_path#"-I"}
zlib_include=$(pkg-config zlib --libs | cut -d ' ' -f 1)
zlib_include=${zlib_include#"-L"}

gmp_path=$(pkg-config gmp --cflags)
gmp_path=${gmp_path#"-I"}
gmp_include=$(pkg-config gmp --libs | cut -d ' ' -f 1)
gmp_include=${gmp_include#"-L"}

LIBRARY_PATH="$zlib_include:$gmp_include" CPATH="$zlib_path:$gmp_path" opam install coq
