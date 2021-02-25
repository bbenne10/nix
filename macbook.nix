{ config, pkgs, ... }: {
  imports = [
    ./lib/macos.nix
  ];

  nixpkgs.overlays = [ (import ./overlay_defs) ];

  home.username = "bbennett37";
  home.homeDirectory = "/Users/bbennett37";
}
