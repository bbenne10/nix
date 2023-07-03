{ pkgs, userName, ... }: {
  environment.systemPackages = with pkgs; [
    terminus_font
    neovim
  ];
  networking.hostName = "home-server";
  networking.nameservers = [ "192.168.1.142" "1.1.1.1" ];
  networking.networkmanager.enable = true;

  users.users.${userName} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "video" "networkmanager" ];
    hashedPassword = "$6$hc672tTQXjHQV$xOGejAjJAdP3VhKMAHCZ2J8G0mj2mjrYS7l4hkq6fVRlLygWplZeem4LX0MEdGGBsGaqClLUc6Z4fkRsfROYB/";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQtmdA5vhoNcN14PeFS80Y++BVPSBJKajg1hlqdr5dwhr+Ug6zvUHVpJy36FZvM6VL0t/cB4GwFpv9B+tHkECTfHQgQLvQ1pQIua5ByEf3hhc5owVWA3WOQa9E92F+PFR/AjNJHaQqSAZevYobxRT03r4fCkwaODXWuttz0314hV0HJMZPXZQxHrPEpBBmm7AcetWsu4zExCwwEODK1aT7WvDUp6CvIQaAqRSkfZQhirD//E7XgChTvVcVbjVV2E6akSOPr0cAZb08P6/XjXemddV3ohJtgzGVB8zixCf34Z53etD4j6MaVWjiRmv5J2Pffc7Kzwwdjs+LFkSr328L cardno:000606534762"
    ];
  };

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  services.openssh.enable = true;
  console = {
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
    keyMap = "us";
    earlySetup = true;
  };

  home-manager.users.${userName} = {
    services.syncthing = {
      enable = true;
    };
  };

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  security.pam.services.gdm.enableGnomeKeyring = true;
  services.xserver.desktopManager.gnome.enable = true;

  # TODO: Maybe later
  # hardware.pulseaudio.enable = false;
  # security.rtkit.enable = true;
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Stolen from hardware-configuration (autogen'd by nix installer)
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/2f9674e3-bbb7-4bd9-bd42-ba2fd012ae2c";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/D355-ACB3";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/7022856f-0672-4625-b503-1b8d84db89e3"; }
  ];
}