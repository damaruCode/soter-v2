# soter-v2
## How to run it the nix way
Installing the [nix](https://nixos.org/download/) package manager allows for easy dependency management and
prevents polluting your own system with packages you otherwise wouldn't need.

### Nix Dev Shell
To create an environment with all the necessary dependencies call:
```
cd <repo> && nix develop
``` 
This will download the packages into the so called "nix store" and symlink them into the
created environment.

### Run soter-v2 (ICFA Examples)
Cargo currently does the entire work. To run one of the examples from "icfa\_examples" execute the following:
```
cargo run --release icfa_examples/<FILE>.erl -a standard -t 0
```
which runs <FILE>.erl with a standard 0-CFA abstraction. The following commandline flags and arguments are defined:
- `--abstraction, -a <ABSTRACTION>`: the abstraction to be used for the analysis
- `--time-depth, -t <NUMBER>`: the $k$ of a $k$-CFA analysis; regulates the context-sensitivity of the analysis
- `--log, -l`: turns logging on. Logs are saved in the `logs` subdirectory of \<OUTPUT\_DIR\> (default: out)
- `--export-graph`: exports a a .dot file for graphviz and a compiled .svg file. These files are saved in the `graphs` subdirectory, grouped by the name of the input file <FILE>.
- `--output-dir, -o <OUTPUT_DIR>`: sets the directory for logging outputs and graphs.
- `--stop-time`: stops the time the analyzer took to analyse the input file.
