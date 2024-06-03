{ pkgs, userName, dwl-src, ... }:
let dwl = (pkgs.callPackage ../derivations/dwl.nix { dwl-src = dwl-src; }); in
{
  boot.kernelPackages = pkgs.linuxPackages_zen;
  hardware.system76.enableAll = true;
  environment.systemPackages = with pkgs; [
    wlr-randr
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
    home.packages = with pkgs; [
      bemenu
      firefox
      libnotify
      xdg-utils
      xdg-desktop-portal
      dwl
      wlr-randr
      wl-clipboard
      wofi
    ];
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
      pinentryFlavor = "qt";
      # Disabled until  the following is fixed upstream
      # (and maybe backported, depending on time-table?)
      # https://github.com/nix-community/home-manager/issues/4804
      # extraConfig = ''
      #   pinentry-program ${pkgs.pinentry-bemenu}/bin/pinentry-bemenu
      # '';
    };

    services.kanshi = {
      enable = true;
      profiles = {
        default = {
          outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
            }
          ];
        };
        docked = {
          outputs = [
            {
              criteria = "eDP-1";
              status = "disable";
            }
            {
              criteria = "HDMI-A-2";
              status = "enable";
            }
          ];
        };
      };
    };

    programs.waybar = {
      enable = true;
      settings = {
        mainBar = {
          spacing = 4;
          margin = "10";
          modules-left = [
            "clock"
            "dwl/tags"
          ];
          modules-center = [ ];
          modules-right = [
            "pulseaudio"
            "network"
            "cpu"
            "memory"
            "temperature"
            "backlight"
            "battery"
          ];

          keyboard-state = {
            numlock = true;
            capslock = true;
            format = "{icon} {name}";
            format-icons = {
              locked = "";
              unlocked = "";
            };
          };
          "dwl/tags" = {
            num-tags = 4;
            tag-labels = [
              ""
              ""
              ""
              ""
            ];
          };
          mpd = {
            format = " {stateIcon} {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}{artist} - {album} - {title} ({elapsedTime:%M:%S}/{totalTime:%M:%S}) ⸨{songPosition}|{queueLength}⸩ {volume}%";
            format-disconnected = " Disconnected";
            format-stopped = " {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}Stopped";
            unknown-tag = "N/A";
            interval = 2;
            consume-icons = {
              on = " ";
            };
            random-icons = {
              off = "<span color=\"#f53c3c\"></span> ";
              on = " ";
            };
            repeat-icons = {
              on = " ";
            };
            single-icons = {
              on = "1 ";
            };
            state-icons = {
              paused = "";
              playing = "";
            };
            tooltip-format = "MPD (connected)";
            tooltip-format-disconnected = "MPD (disconnected)";
          };
          clock = {
            timezone = "America/New_York";
            tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
            format-alt = "{:%Y-%m-%d}";
            format = "{:%a; %b %e @ %I:%M %p}";
          };
          cpu = {
            format = " {usage}%";
            tooltip = false;
          };
          memory = {
            format = " {}%";
          };
          temperature = {
            # thermal-zone = 2;
            # hwmon-path = "/sys/class/hwmon/hwmon2/temp1_input";
            # format-critical = "{temperatureC}°C {icon}";
            critical-threshold = 80;
            format = "{icon} {temperatureC}°C";
            format-icons = [ "" "" "" ];
          };
          backlight = {
            # device = "acpi_video1";
            format = "{icon} {percent}%";
            format-icons = [ "" "" "" "" "" "" "" "" "" ];
          };
          battery = {
            states = {
              # good = 95;
              warning = 30;
              critical = 15;
            };
            format = "{icon} {capacity}%";
            format-charging = " {capacity}% ";
            format-plugged = " {capacity}% ";
            format-alt = "{icon} {icon}";
            # format-good = ""; 
            # format-full = "";
            format-icons = [ "" "" "" "" "" ];
          };
          network = {
            # interface = "wlp2*";
            # (Optional) To force the use of this interface
            format-wifi = " {essid} ({signalStrength}%) ";
            format-ethernet = "{ipaddr}/{cidr}";
            tooltip-format = "{ifname} via {gwaddr}";
            format-linked = "{ifname} (No IP)";
            format-disconnected = "⚠ Disconnected";
            format-alt = "{ifname}: {ipaddr}/{cidr}";
          };
          pulseaudio = {
            # scroll-step = 1; # %, can be a float
            format = "{icon} {volume}% {format_source}";
            format-bluetooth = "{icon} {volume}% {format_source}";
            format-bluetooth-muted = "{icon}  {format_source}";
            format-muted = " {format_source}";
            format-source = " {volume}% ";
            format-source-muted = "";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = [ "" "" "" ];
            };
            on-click = "${pkgs.pavucontrol}";
          };
        };
      };
      style = ../conf.d/waybar_style.css;
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
    cue = true;
  };

  security.pam.services.swaylock = { };
}

