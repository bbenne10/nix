{ pkgs, userName, ... }:
let
  riverSession = pkgs.writeScriptBin "river-session" ''
    # Session
    export XDG_SESSION_TYPE=wayland
    export XDG_SESSION_DESKTOP=river
    export XDG_CURRENT_DESKTOP=river

    # Wayland stuff
    export MOZ_ENABLE_WAYLAND=1
    export QT_QPA_PLATFORM=wayland
    export SDL_VIDEODRIVER=wayland
    export _JAVA_AWT_WM_NONREPARENTING=1

    dbus-update-activation-environment --systemd XDG_CURRENT_DESKTOP XDG_SESSION_TYPE
    exec "${pkgs.river}/bin/river"
  '';
in
{
  boot.kernelPackages = pkgs.linuxPackages_zen;
  hardware.system76.enableAll = true;
  environment.systemPackages = with pkgs; [
    pmutils
    terminus_font
    gnomeExtensions.pop-shell
    gnomeExtensions.gsconnect
    gnomeExtensions.caffeine
    gnomeExtensions.media-controls
    gnome.gnome-tweaks
    gthumb
  ];
  environment.gnome.excludePackages = (with pkgs; [
    gnome-photos
    gnome-tour
  ]) ++ (with pkgs.gnome; [
    eog
    epiphany
    geary
    gedit
    gnome-characters
    gnome-initial-setup
    gnome-music
    iagno
    tali
    yelp
  ]);
  programs.dconf.enable = true;
  services.gnome.gnome-keyring.enable = true;

  networking.hostName = "bennett-laptop";
  networking.nameservers = [ "192.168.1.142" "1.1.1.1" ];
  networking.networkmanager.enable = true;

  users.users.${userName} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "vboxusers" "video" ];
    hashedPassword = "$6$hc672tTQXjHQV$xOGejAjJAdP3VhKMAHCZ2J8G0mj2mjrYS7l4hkq6fVRlLygWplZeem4LX0MEdGGBsGaqClLUc6Z4fkRsfROYB/";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQtmdA5vhoNcN14PeFS80Y++BVPSBJKajg1hlqdr5dwhr+Ug6zvUHVpJy36FZvM6VL0t/cB4GwFpv9B+tHkECTfHQgQLvQ1pQIua5ByEf3hhc5owVWA3WOQa9E92F+PFR/AjNJHaQqSAZevYobxRT03r4fCkwaODXWuttz0314hV0HJMZPXZQxHrPEpBBmm7AcetWsu4zExCwwEODK1aT7WvDUp6CvIQaAqRSkfZQhirD//E7XgChTvVcVbjVV2E6akSOPr0cAZb08P6/XjXemddV3ohJtgzGVB8zixCf34Z53etD4j6MaVWjiRmv5J2Pffc7Kzwwdjs+LFkSr328L cardno:000606534762"
    ];
  };

  users.groups.keyd = { };
  systemd.services.keyd = {
    description = "System-wide remapping daemon";
    requires = [ "local-fs.target" ];
    after = [ "local-fs.target" ];
    wantedBy = [ "sysinit.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.keyd}/bin/keyd";
      Type = "simple";
    };
  };

  networking.firewall.allowedTCPPortRanges = [
    {
      from = 1714;
      to = 1764;
    }
  ];
  networking.firewall.allowedUDPPortRanges = [
    {
      from = 1714;
      to = 1764;
    }
  ];

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    keyMap = "us";
  };
  home-manager.users.${userName} = {
    home.packages = with pkgs; [
      firefox
      libnotify
      spot
      xdg-utils
    ];

    gtk = {
      enable = true;
      theme = {
        name = "Nordic";
        package = pkgs.nordic;
      };
      cursorTheme = {
        name = "Vanilla-DMZ";
        package = pkgs.vanilla-dmz;
      };
      font = {
        name = "Recursive Sans Linear Static";
        size = 10;
      };
      iconTheme = {
        name = "Zafiro-icons-Dark";
        package = pkgs.zafiro-icons;
      };
    };

    services.syncthing = {
      enable = true;
    };
  };

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  security.pam.services.gdm.enableGnomeKeyring = true;
  services.xserver.desktopManager.gnome.enable = true;

  environment.etc."keyd/default.conf".source = ../conf.d/keyd_config;

  services.pcscd.enable = true;

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
    powertop = {
      enable = true;
    };
  };
  sound.enable = true;

  hardware.pulseaudio.enable = true;

  environment.variables = {
    GDK_SCALE = "2";
  };

  services.upower.enable = true;

  console.earlySetup = true;
}

