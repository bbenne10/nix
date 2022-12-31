{ pkgs, userName, ...}: {
  environment.systemPackages = with pkgs; [
    pmutils
    terminus_font
  ];

  networking.hostName = "bennett-laptop"; 
  networking.nameservers = [ "192.168.1.142" ];

  users.users.${userName} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "vboxusers" "video"];
    hashedPassword = "$6$hc672tTQXjHQV$xOGejAjJAdP3VhKMAHCZ2J8G0mj2mjrYS7l4hkq6fVRlLygWplZeem4LX0MEdGGBsGaqClLUc6Z4fkRsfROYB/";
  };

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    keyMap = "us";
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
            on-click = "pavucontrol";
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
  sound.enable = true;

  hardware.pulseaudio.enable = true;

  environment.variables = {
    GDK_SCALE = "2";
  };

  services.upower.enable = true;
}
