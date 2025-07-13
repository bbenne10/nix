{ pkgs, userName, ... }:
{

  # As of 1/21/25, systemd-networkd
  # and nm-online target are collectively broken.
  # Just disable online checks.
  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.network.wait-online.enable = false;

  boot.kernelPackages = pkgs.linuxPackages_zen;
  hardware.system76.enableAll = true;
  environment.systemPackages = [ 
    pkgs.pmutils
    pkgs.terminus_font
    # for lanzaboote
    pkgs.sbctl
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

  systemd.services.keyd.serviceConfig.CapabilityBoundingSet = [
    "CAP_SETGID"                                               
  ];  

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

  services.pulseaudio.enable = false;
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

  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  hardware.graphics.enable = true;

  users.defaultUserShell = pkgs.zsh;

  boot = {
    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ ];
      luks = {
        devices = {
         root = {
            # device = "/dev/disk/by-label/NixOS";
            device = "/dev/disk/by-uuid/2ffb7979-b188-48a7-9570-4d42ed89a0c7";
          };
          home = {
            # device = "/dev/disk/by-label/NixOS-Home";
            device = "/dev/disk/by-uuid/9e74fd32-2779-4282-81bd-28f6745bbdf3";
          };
        };
        };
      };
    kernelModules = [ "kvm-intel" "i915" ];
    loader = {
      # lanzaboote replaces systemd-boot
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = true;
    };

    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };

  };

  fileSystems = {
    "/" = {
      label = "NixOS";
      fsType = "btrfs";
      options = ["subvol=@"];
    };

    "/nix" = {
      label = "NixOS";
      fsType = "btrfs";
      options = ["subvol=@nix"];
    };

    "/etc/nixos" = {
      label = "NixOS";
      fsType = "btrfs";
      options = ["subvol=@nix-config"];
    };

    "/var/log" = {
      label = "NixOS";
      fsType = "btrfs";
      options = ["subvol=@log"];
    };

    "/home" = {
      label = "NixOS-Home";
      fsType = "btrfs";
    };
  };
}

