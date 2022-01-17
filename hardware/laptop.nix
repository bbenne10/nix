{ config, lib, pkgs, pkgsForSystem, ... }:

{
  imports =
    [
      # <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  hardware.enableRedistributableFirmware = true;
  users.defaultUserShell = pkgsForSystem.zsh;
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

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.opengl.enable = true;

  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.wireless.extraConfig = ''
    ctrl_interface=/run/wpa_supplicant
    ctrl_interface_group=wheel
  '';
  networking.wireless.networks.Bennett-Wireless.pskRaw = "06d6f78b406ec252570423e6cc3892ae48a8f79567374ae5421161124e6f9850";
  networking.wireless.networks."CenturyLink4779-5G".pskRaw = "01ea6ec30d029c7600d46529b2d25876127b9fad30940ecb2c23600eb75e7007";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp58s0f1.useDHCP = true;
  networking.interfaces.wlp59s0.useDHCP = true;
  
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
}
