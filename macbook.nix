{ config, pkgs, ... }: {
  imports = [ ./lib/common.nix ];
  nixpkgs.overlays = [ (import ./overlay_defs) ];

  home.username = "bbennett37";
  home.homeDirectory = "/Users/bbennett37";

  programs.firefox = {
    enable = true;
    package = pkgs.Firefox;
  };
}
