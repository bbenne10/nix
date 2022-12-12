{ pkgsForSystem, userName, ...}: {

  environment.systemPackages = with pkgsForSystem; [
    bash
    cachix
    pmutils
    terminus_font
  ];

  home-manager.users.${userName} = {
    home.packages = with pkgsForSystem; [
      brightnessctl
      foot
      kanshi
      libnotify
      mako
      pamixer
      playerctl
      qutebrowser
      river
      spotify
      wofi
      yambar
    ];

    home.file.".config/river/init".source = ./../conf.d/river_init;
    home.file.".config/foot/foot.ini".source = ./../conf.d/foot.ini;
    home.file.".config/yambar/config.yml".source = ./../conf.d/yambar.yml;
    home.file.".config/kanshi/config".source = ./../conf.d/kanshi_config;
    home.file.".config/qutebrowser/autoconfig.yml".source = ./../conf.d/qutebrowser_autoconfig.yml;
  };

  networking.hostName = "bennett-laptop"; 
  networking.nameservers = [ "192.168.1.142" ];

  programs.adb.enable = true;
  services.udev.packages = [ pkgsForSystem.android-udev-rules ];
  users.users.${userName} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "adbusers"];
    hashedPassword = "$6$hc672tTQXjHQV$xOGejAjJAdP3VhKMAHCZ2J8G0mj2mjrYS7l4hkq6fVRlLygWplZeem4LX0MEdGGBsGaqClLUc6Z4fkRsfROYB/";
  };

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "${pkgsForSystem.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    keyMap = "us";
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
    _JAVA_OPTIONA = "-Dsun.java2d.uiScale=2";
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgsForSystem.cage}/bin/cage -s -- ${pkgsForSystem.greetd.gtkgreet}/bin/gtkgreet";
      };
    };
  };

  environment.etc."greetd/environments".text = ''
    dbus-run-session river
    zsh
  '';
  services.upower.enable = true;
}
