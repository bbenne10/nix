{
  lib,
  pkgs,
  ...
}:
let
  airsonicRefix = pkgs.callPackage ../../derivations/airsonic-refix { };
in
{
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    (builtins.elem (lib.getName pkg) [
      "minecraft-server"
    ]);
  environment.systemPackages = [
    pkgs.terminus_font
    pkgs.neovim
  ];

  networking.hostName = "home";
  networking.useNetworkd = true;

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

  users.groups.media = {
    members = [
      "bryan"
    ];
  };

  users.users.media = {
    isSystemUser = true;
    description = "cross-service media user";
    group = "media";
  };

  services.minecraft-server = {
    enable = true;
    eula = true;
    openFirewall = true;
  };

  networking.firewall.allowedTCPPorts = [
    80
  ];

  networking.firewall.allowedUDPPorts = [
    22000
    21027
  ];

  services.nginx = {
    enable = true;
    virtualHosts = {
      "gonic.*" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:4747/";
          proxyWebsockets = true;
        };
      };
      "prowlarr.*" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:9696/";
          proxyWebsockets = true;
        };
      };
      "lidarr.*" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:8686/";
          proxyWebsockets = true;
        };
      };
      "deluge.*" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:8112/";
          proxyWebsockets = true;
        };
      };
      "music.*" = {
        root = "${airsonicRefix}";
      };
    };
  };

  services.tailscale.enable = true;

  services.lidarr.enable = true;
  services.lidarr.openFirewall = true;

  services.prowlarr.enable = true;
  services.prowlarr.openFirewall = true;

  services.deluge = {
    enable = true;
    web.enable = true;
    package = pkgs.deluged;
  };

  services.gonic = {
    enable = true;
    settings = {
      music-path = "/media/music";
      podcast-path = "/media/podcasts";
      playlists-path = "/media/playlists";
      scan-at-start-enabled = true;
      scan-watcher-enabled = true;
      podcast-purge-age = 30;
    };
  };

  # Stolen from hardware-configuration (autogen'd by nix installer)
  nixpkgs.hostPlatform = "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = true;

  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ehci_pci"
    "ahci"
    "usb_storage"
    "usbhid"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];

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
    fsType = "ext4";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/7022856f-0672-4625-b503-1b8d84db89e3"; }
  ];
}
