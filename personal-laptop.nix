{ config, pkgs, ... }: {
  imports = [ ./linux.nix ];

  home.username = "bryan";
  home.stateVersion = "20.09";

  # Overrides for HiDPI screen
  xresources.properties = { "Xft.dpi" = "192"; };
  xsession.pointerCursor.size = 48;
}
