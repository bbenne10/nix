{ config, lib, pkgs, userName, ... }: {
  environment.systemPackages = with pkgs; [
    terminus_font
    neovim
  ];
  networking.hostName = "home-server";
  networking.networkmanager.enable = true;

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

  services.jellyfin.enable = true;
  services.jellyfin.openFirewall = true;

  services.syncthing = {
    enable = true;
    guiAddress = "0.0.0.0:8384";
    dataDir = "/media";
    configDir = "/media/.syncthing_config";
    overrideDevices = true;
    overrideFolders = true;
    devices = {
      "bryan-phone" = { id = "GH3EYR3-V72E72X-3VILKVK-S5SO4PA-LDJZWB7-W4CG7L7-3KYRRLK-FPMKDAE";};
    };
    folders = {
      "Music" = {
        path = "/media/Music";
        devices = ["bryan-phone"];
      };
    };
  };

  services.minecraft-server = {
    enable = true;
    eula = true;
    openFirewall = true;
  };
  networking.firewall.allowedTCPPorts = [ 8384 ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];


  # Stolen from hardware-configuration (autogen'd by nix installer)
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "ntfs3" ];

  users.groups.media.members = [ "bryan" "jelyfin" "syncthing" ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/2f9674e3-bbb7-4bd9-bd42-ba2fd012ae2c";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/D355-ACB3";
    fsType = "vfat";
  };

  fileSystems."/media" = {
    device = "/dev/sdb1";
    fsType = "ntfs3";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/7022856f-0672-4625-b503-1b8d84db89e3"; }
  ];
}
