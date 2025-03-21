{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    erlang_27

    rust_analayzer
    rustc
    cargo
  ];
}
