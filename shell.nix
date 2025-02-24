{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    cargo
    rust-analyzer
    rustc
  ];
}
