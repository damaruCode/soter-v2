# soter-v2
## How to run it the nix way
Installing the [nix](https://nixos.org/download/) package manager allows for easy dependency management and
prevents polluting your own system with packages you otherwise wouldn't need.

### Nix shell
To create an environment with all the necessary dependencies call:
```
cd <repo> && nix-shell
``` 
This will download the packages into the so called "nix store" and symlink them into the
created environment.

### Run soter-v2
Cargo currently does the entire work. To run one of the test cases "fib.erl" execute the following:
```
cargo run --release test/fib.erl
```
Rust starts the erlang processies that compile and run the erlang code in "<repo>/erlang".

### Output
"<repo>/core.json"
