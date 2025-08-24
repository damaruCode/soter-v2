#!/bin/bash
cargo build --release
for file in icfa_examples/*.erl; do
  echo "-------------------------"
  echo "$file"
  echo "-------------------------"
  target/release/soter-v2 "$file" -a standard -t 0 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/Standard (0) - \1/"
  target/release/soter-v2 "$file" -a standard -t 1 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/Standard (1) - \1/"
  target/release/soter-v2 "$file" -a p4f -t 0 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/P4F (0) - \1/"
  target/release/soter-v2 "$file" -a icfa -t 0 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/ICFA (0) - \1/"
done

for dir in out/graphs/*; do
  for file in "$dir"/*.dot; do
    dot -Tsvg "$file" > "$file".svg
  done
done
