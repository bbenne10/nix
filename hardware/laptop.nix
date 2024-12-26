{ config, lib, pkgs, ... }:

{
  imports =
    [
      # <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  hardware.enableRedistributableFirmware = true;
  users.defaultUserShell = pkgs.zsh;
  boot.kernelParams = [
    # Does not work to fix flicker
    # "i915.enable_fbc=0"
    # "intel_idle.max_cstate=4"
    "i915.enable_psr=0"

  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "i915" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/nvme0n1p2";
      fsType = "btrfs";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/AF7F-1C95";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/d6bc2fc8-e9bf-4d63-a0cc-18d9f30b715b";
      fsType = "ext4";
    };

  swapDevices = [
    { device = "/dev/nvme0n1p1"; }
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.graphics.enable = true;

  networking.interfaces.enp58s0f1.useDHCP = true;
  networking.interfaces.wlp59s0.useDHCP = true;

}
