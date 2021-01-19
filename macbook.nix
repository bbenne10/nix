{ config, pkgs, ... }: {
  imports = [ ./lib/common.nix ];
  nixpkgs.overlays = [ (import ./overlay_defs) ];

  home.username = "bbennett37";
  home.homeDirectory = "/Users/bbennett37";

  programs.firefox = {
    # Let definition of common override everything but package here
    package = pkgs.Firefox;
  };

  home.packages = with pkgs; [
    (pkgs.lowPrio emacsMacport)
  ];
}
