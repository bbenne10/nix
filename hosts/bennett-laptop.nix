{ pkgs, userName, ... }:
{
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

