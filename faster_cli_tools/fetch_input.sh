#!/bin/bash

set -e

echo "downloading data file..."
wget https://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-1gram-20120701-0.gz -O input.txt.gz
echo "extracting input.txt.gz to input.txt ..."
gunzip input.txt.gz
echo "done"
