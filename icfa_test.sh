#!/bin/bash
cargo build --release
for file in icfa_examples/*.erl; do
  echo "-----------------------------"
  echo "$file"
  echo "-----------------------------"
  target/release/soter-v2 "$file" -a standard -t 0 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/Standard (0) - \1/"
  target/release/soter-v2 "$file" -a standard-v1cfa -t 0 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/StandardV1CFA (0) - \1/"
  target/release/soter-v2 "$file" -a standard -t 1 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/Standard (1) - \1/"
  target/release/soter-v2 "$file" -a p4f -t 0 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/P4F (0) - \1/"
  target/release/soter-v2 "$file" -a icfa -t 0 --export-graph --stop-time | sed -r "s/Time: ([0-9]+ ns)/ICFA (0) - \1/"
done

# echo ""
# echo ""
# echo "-----------------------------"
# echo "Compiling DOT files to SVG..."
# echo "-----------------------------"
# for dir in out/graphs/*; do
#   for file in "$dir"/*.dot; do
#     dot -Tsvg "$file" > "${file%.dot}.svg"
#     echo "$file -> $file.svg"
#   done
# done

# For future reference:
# unflatten -f -l 3 -c 4 out/graphs/id/id.icfa.0.erl.dot out/graphs/id_tuple/id_tuple.icfa.0.erl.dot | dot | gvpack -array_t6 | neato -s -n2 -Tsvg > c.standard.0.erl.svg
