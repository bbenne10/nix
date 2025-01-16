{ pkgs, ... }: {

  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  hardware.graphics.enable = true;
  hardware.system76.enableAll = true;

  environment.systemPackages = [
    # for lanzaboote
    pkgs.sbctl
  ];

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
    # use 6.11 until system76 modules work on 6.12
    kernelPackages = pkgs.linuxKernel.packages.linux_6_11;
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

  networking.interfaces = {
    en58s0f1 = {
      useDHCP = true;
    }; 
    wlp59s0 = {
      useDHCP = true;
    }; 
  };

}
