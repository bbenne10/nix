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
