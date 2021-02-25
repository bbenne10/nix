{ config, pkgs, ... }: {
  imports = [ ./common.nix ];

  xsession = {
    enable = true;
    windowManager = { command = "${pkgs.dwm}/bin/dwm"; };
    initExtra = ''
      . ~/.fehbg;
    '';
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
    };
  };

  home.packages = with pkgs; [
      acpitool
      brightnessctl
      dwm
      feh
      gcc
      gnum4
      gnumake
      htop
      i3lock
      jetbrains-mono
      pciutils
      picom
      pmutils
      pulseaudio-ctl
      rofi
      rofi-pass
      weechat
      xss-lock
    ];

  programs.dwm = {
    enable = true;
    package = Dwm;
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  services.picom = {
    enable = true;
    experimentalBackends = true;
    fade = true;
    vSync = true;
    blur = true;
  };
}
