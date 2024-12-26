{ config, lib, pkgs, userName, ... }: {
  environment.systemPackages = builtins.attrValues
    {
      inherit (pkgs)
        terminus_font
        neovim
        ;
    };
  networking.hostName = "home-server";
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
      # "syncthing"
      "bryan"
      "mopidy"
    ];
  };

  users.users.media = {
    isSystemUser = true;
    description = "cross-service media user";
    group = "media";
    extraGroups = [ "mopidy" ];
  };

  services.syncthing = {
    enable = false;
    user = "media";
    group = "media";
    guiAddress = "0.0.0.0:8384";
    dataDir = "/media";
    configDir = "/media/.syncthing_config";
    overrideDevices = true;
    overrideFolders = true;
    settings = {
      devices = {
        "bryan-phone" = { id = "GH3EYR3-V72E72X-3VILKVK-S5SO4PA-LDJZWB7-W4CG7L7-3KYRRLK-FPMKDAE"; };
      };
      folders = {
        "Music" = {
          path = "/media/Music";
          devices = [ "bryan-phone" ];
        };
        "Camera" = {
          path = "/media/Camera";
          devices = [ "bryan-phone" ];
        };
      };
    };
  };

  services.minecraft-server = {
    enable = true;
    eula = true;
    openFirewall = true;
  };
  networking.firewall.allowedTCPPorts = [
    # 8384 # syncthing
    80 # nginx name-based routing
    5030 # slskd
    6680 # mopidy web
  ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];

  services.snapserver = {
    enable = true;
    codec = "flac";
    openFirewall = true;
    streams = {
      mopidy = {
        type = "pipe";
        location = "/run/snapserver/mopidy";
      };
    };
  };

  services.nginx = {
    enable = true;
    virtualHosts = {
      "music.*" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:4533/";
          proxyWebsockets = true;
        };
      };
    };
  };

  services.slskd = {
    enable = true;
    group = "media";
    user = "media";
    openFirewall = true;
    domain = "slskd.*";
    environmentFile = "/etc/slskdEnv";
    settings.shares.directories = [ "/media/Music" ];
  };

  services.navidrome = {
    enable = true;
    user = "media";
    group = "media";
    settings.MusicFolder = "/media/Music";
  };

  services.dashy = {
    enable = true;
    virtualHost = {
      enableNginx = true;
      domain = "/";
    };
  };

  services.tailscale.enable = true;

  # Stolen from hardware-configuration (autogen'd by nix installer)
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
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
