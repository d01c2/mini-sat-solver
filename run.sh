#!/bin/bash

CURRENT_DIR=$(pwd)

cd datasets || { echo "Error: datasets directory not found"; exit 1; }

tar -xzf "uf20-91.tar.gz"
tar -xzf "uf50-218.tar.gz"
tar -xzf "uuf50-218.tar.gz"

cd "$CURRENT_DIR"

sbt run

cd datasets || { echo "Error: datasets directory not found"; exit 1; }

rm -rf *.cnf
rm -rf ./UUF50.218.1000

cd "$CURRENT_DIR"
