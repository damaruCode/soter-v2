{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    rust_analayzer
    erlang_27
    rustc
    cargo
  ];
}
