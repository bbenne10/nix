{ pkgs, userName, dwl-custom, ... }:
let
  dwl_status=dwl-custom.packages.x86_64-linux.dwls;
in {
  boot.kernelPackages = pkgs.linuxPackages_zen;
  hardware.system76.enableAll = true;
  environment.systemPackages = with pkgs; [
    pmutils
    terminus_font
  ];

  networking.hostName = "bennett-laptop";
  networking.networkmanager.enable = true;

  programs.dconf.enable = true;

  users.groups.keyd = { };
  services.tailscale = {
    enable = true;
    openFirewall = true;
  };

  services.keyd = {
    enable = true;
    keyboards.default = {
      ids = [ "*" ];
      settings = {
        main = {
          insert = "S-insert";
          capslock = "overload(control, menu)";
        };
      };
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
  home-manager.users.${userName} = {
    home.packages = (builtins.attrValues {
      inherit dwl_status;
      inherit (pkgs)
        bemenu
        libnotify
        xdg-utils
        xdg-desktop-portal
        wlr-randr
        wl-clipboard
      ;
    });
    home.file.u2f_keys = {
      target = ".config/Yubico/u2f_keys";
      text = "bryan:6wdZOqyH9e2yqQZxcIoWoh2Ns+asTlhxBnzwGJvLP4c6MkCdM6jht2hQ8hwVWaxcv/1W43g0Ct1kJOVZKXruyg==,pvVwE1EtFLyuofbyWwX93grSfhDZa1JWPmkPybtfovzEFoqUCIPoeH0oJx+subMVLutVokwtKP6DgJP8PTce2w==,es256,+presence";
    };

    gtk = {
      enable = true;
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

    services.gpg-agent = {
      enable = true;
      enableZshIntegration = true;
      enableScDaemon = true;
      enableSshSupport = true;
      pinentryPackage = pkgs.pinentry-bemenu;
    };

    services.kanshi = {
      enable = true;
      settings = [
        {
          profile.name = "default";
          profile.outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
            }
          ];
        }
        {
          profile.name = "docked";
          profile.outputs = [
             {
               criteria = "eDP-1";
               status = "disable";
             }
             {
               criteria = "HDMI-A-2";
               status = "enable";
             }
           ];
        }
      ];
    };
    };


  services.pcscd.enable = true;
  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
    powertop = {
      enable = true;
    };
  };

  hardware.bluetooth.enable = true;
  security.rtkit.enable = true;

  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  environment.variables = {
    GDK_SCALE = "2";
  };

  services.upower.enable = true;

  security.pam.u2f = {
    enable = true;
    settings.cue = true;
  };

  security.pam.services.swaylock = { };
}

