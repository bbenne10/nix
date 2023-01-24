{ pkgs, userName, ... }:
let riverSession = pkgs.writeScriptBin "river-session" ''
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
''; in
{
  environment.systemPackages = with pkgs; [
    pmutils
    terminus_font
  ];

  networking.hostName = "bennett-laptop";
  networking.nameservers = [ "192.168.1.142" ];

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
  programs.dconf.enable = true;

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    keyMap = "us";
  };
  home-manager.users.${userName} = {
    home.packages = with pkgs; [
      brightnessctl
      pamixer
      pavucontrol
      playerctl
      qutebrowser
      river
      wofi
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
        name = "Recursive Mono Linear Static";
        size = 10;
      };
    };

    # Set up a graphical target session for river 
    systemd.user.targets.river-session = {
      Unit = {
        Description = "river compositor session";
        Documentation = [ "man:systemd.special(7)" ];
        BindsTo = [ "graphical-session.target" ];
        Wants = [ "graphical-session-pre.target" ];
        After = [ "graphical-session-pre.target" ];
      };
    };

    programs.foot = {
      enable = true;
      settings = {
        main = {
          font = "Recursive Mono Linear Static:size=10";
        };
        scrollback = {
          lines = 10000;
        };
      };
    };
    programs.waybar = {
      enable = true;
      systemd = {
        enable = true;
        target = "river-session.target";
      };
      settings = {
        mainBar = {
          spacing = 4;
          margin = "10";
          modules-left = [
            "clock"
            "river/tags"
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
          "river/mode" = {
            format = "<span style=\"italic\">{}</span>";
          };
          "river/tags" = {
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
            format-wifi = "{essid} ({signalStrength}%) ";
            format-ethernet = "{ipaddr}/{cidr} ";
            tooltip-format = "{ifname} via {gwaddr} ";
            format-linked = "{ifname} (No IP) ";
            format-disconnected = "Disconnected ⚠";
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
    services.kanshi = {
      enable = true;
      systemdTarget = "river-session.target";
      profiles = {
        undocked = {
          outputs = [
            {
              criteria = "eDP-1";
              scale = 2.0;
            }
          ];
        };
        docked_home = {
          outputs = [
            {
              criteria = "eDP-1";
              scale = 2.0;
            }
            {
              criteria = "Goldstar Company Ltd LG ULTRAWIDE 0x00001FB7";
              status = "enable";
              position = "1600,0"; # logical pixels - not phsyical ones - account for 2x scale
              scale = 1.0;
            }
          ];
        };
      };
    };
    home.file.".config/river/init".source = ./../conf.d/river_init;
  };

  environment.etc."keyd/default.conf".source = ../conf.d/keyd_config;
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.cage}/bin/cage -s -- ${pkgs.greetd.gtkgreet}/bin/gtkgreet";
      };
    };
  };

  environment.etc."greetd/environments".text = ''
    ${riverSession}/bin/river-session 
    zsh
  '';

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
}
