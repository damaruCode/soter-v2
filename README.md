# soter-v2

## Supported Language Features
Oriented on the rust-representation of the Erlang AST in [`src/ast`](src/ast/mod.rs).

### Legend
- ✅: full support
- ✴️: partial support
- ❌: unsupported

### Expressions
| Construct | Supported | Description |
| :- | :-: | :- |
| [Alias](src/ast/alias.rs) | ❌ | |
| [Apply](src/ast/apply.rs) | ✅ | |
| [Call](src/ast/call.rs) | ✅/✴️ | only for `erlang:self`, `erlang:spawn` and `erlang:send`; no general functionality because soter-v2 only works for one module |
| [Case](src/ast/case.rs)/[Clause](src/ast/clause.rs) | ✅ |  |
| [Fun](src/ast/fun.rs) | ✅ | |
| [Let](src/ast/let.rs) | ✅ | |
| [LetRec](src/ast/let_rec.rs) | ❌ | |
| [MapPair](src/ast/map_pair.rs) | ❌ | |
| [Module](src/ast/module.rs) | ✴️ | |
| [PrimOp](src/ast/prim_op.rs) | ❌ | |
| [Receive](src/ast/receive.rs) | ✅ | |
| [Seq](src/ast/seq.rs) | ✅ | |
| [Try](src/ast/try.rs)/[Catch](src/ast/catch.rs) | ❌ | |
| [Var](src/ast/var.rs) | ✅ | |

### Types of Values
These follow from the supported language constructs.

| Construct | Supported | Description | 
| :- | :-: | :- |
| [Binary](src/ast/binary.rs) | ❌ | |
| [BitStr](src/ast/bit_str.rs) | ❌ | |
| [Cons](src/ast/const.rs) | ✅ | |
| [Literal](src/ast/literal.rs) | ✴️ | only literal strings `'a'`, `'ok'`, and such; full support planned |
| [Opaque](src/ast/opaque.rs) | ❌ | is a compiler internal construct that is supposed to be for debugging so no support planed |
| [Tuple](src/ast/tuple.rs) | ✅ | |
| [Values (i.e. ValueList)](src/ast/values.rs) | ✅ | |

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

## Run soter-v2
Cargo currently does the entire work. To run one of the examples from "tests/benchmarks" execute the following:
```
cargo run --release tests/benchmarks/<FILE>.erl -a standard -t 0
```

which runs <FILE>.erl with a standard 0-CFA abstraction. The following commandline flags and arguments are defined:
- `--abstraction, -a <ABSTRACTION>`: the abstraction to be used for the analysis
- `--time-depth, -t <NUMBER>`: the $k$ of a $k$-CFA analysis; regulates the context-sensitivity of the analysis
- `--log, -l`: turns logging on. Logs are saved in the `logs` subdirectory of \<OUTPUT\_DIR\> (default: out)
- `--export-graph, -g`: exports a a .dot file for graphviz and a compiled .svg file. These files are saved in the `graphs` subdirectory, grouped by the name of the input file <FILE>.
- `--output-dir, -o <OUTPUT_DIR>`: sets the directory for logging outputs and graphs.
- `--stop-time`: stops the time the analyzer took to analyse the input file.

## Run Tests
There are several kinds of tests

### Benchmarks
Runs soter-v2 with a standard 0-CFA abstraction on each file in the `tests/benchmarks` directory only checking that the analysis runs through without crashing:
```
cargo test --test benchmarks
```

### Soundness
Runs soter-v2 on the files in the `tests/soundness` directory and checks the output for expected values in variables and so on:
```
cargo test --test soundness
```

### Unit (upcoming)
*Will check transitions and pattern-matching for correctness.*
