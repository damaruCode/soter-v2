#!/bin/bash
cargo build --release

printf "file, abstraction, time_depth, time, states\n"

abstractions=("standard" "standard-v1cfa" "standard" "p4f" "icfa")
time_depths=(0 0 1 0 0)

for file in icfa_examples/*.erl; do
  for c in "${!abstractions[@]}"; do
    abstraction="${abstractions[$c]}"
    time_depth="${time_depths[$c]}"
    for i in {1..100}; do
      echo "$file, $abstraction, $time_depth, $(target/release/soter-v2 "$file" -a "$abstraction" -t "$time_depth" --export-graph --stop-time | sed -r "s/Time: ([0-9]+), States: ([0-9]+)/\1,\2/")"
    done
  done
done
