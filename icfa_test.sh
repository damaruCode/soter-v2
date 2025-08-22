#!/bin/bash
cargo build --release
for file in icfa_examples/*.erl; do
  echo "-------------------------"
  echo "$file"
  echo "-------------------------"
  target/release/soter-v2 "$file" -a standard -t 0 --export-graph --stop-time
  target/release/soter-v2 "$file" -a standard -t 1 --export-graph --stop-time
  target/release/soter-v2 "$file" -a p4f -t 0 --export-graph --stop-time
  target/release/soter-v2 "$file" -a icfa -t 0 --export-graph --stop-time
done
