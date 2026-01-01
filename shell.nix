{ pkgs ? import <nixpkgs> {} }:
let p = pkgs; in p.mkShell {
  nativeBuildInputs = [ p.nasm p.qemu_test ];
}
