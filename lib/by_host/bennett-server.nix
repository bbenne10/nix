{ config, lib, pkgs, website, ... }:
let
  hostName = "bryan-bennett.com";
  websitePkg = website.packages.${pkgs.system}.website;
in
{
  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = false;
  networking.useNetworkd = true;
  networking.hostName = "bennett-server";
  networking.domain = "";
  services.openssh.enable = true;

  boot.loader.grub.device = "nodev";
  fileSystems."/" = { device = "/dev/vda3"; fsType = "ext4"; };
  swapDevices = [{ device = "/dev/vda2"; }];

  services.fail2ban.enable = true;

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "${hostName}" = {
        root = "${websitePkg}";
        enableACME = true;
        forceSSL = true;
        locations = {
          "/robots.txt" = {
            extraConfig = ''
              rewrite ^/(.*) $1;
              return 200 "User-agent: *\nDisallow: /";
            '';
          };
        };
      };

      "pass.${hostName}" = {
        forceSSL = true;
        enableACME = true;
        locations = {
          "/" = {
            proxyPass = "http://localhost:8000/";
          };
          "/robots.txt" = {
            extraConfig = ''
              rewrite ^/(.*) $1;
              return 200 "User-agent: *\nDisallow: /";
            '';
          };
        };
      };
    };
  };

  services.vaultwarden.enable = true;
  services.vaultwarden.config = {
    SIGNUPS_ALLOWED = false; ## DISABLE IF WE NEED TO REDEPLOY?
    DOMAIN = "https:////pass.${hostName}";
  };

  networking.firewall.allowedTCPPorts = [
    # nginx
    80
    443
  ];

  security.acme = {
    defaults = {
      email = "Bryan.Bennett@protonmail.com";
    };
    acceptTerms = true;
    certs = {
      "${hostName}" = {
        extraDomainNames = [ "pass.${hostName}" "files.${hostName}" ];
      };
    };
  };

  ## Stolen from qemu-guest.nix
  ## Added here until I can figure out way to get this as an input rather than a module import
  ## When handling: diff against source module.
  ## Diff is from nixos-infect and will need to be handled somehow
  boot.initrd.availableKernelModules = [
    "ata_piix"
    "uhci_hcd"
    "xen_blkfront"
    "vmw_pvscsi"
    "virtio_net"
    "virtio_pci"
    "virtio_mmio"
    "virtio_blk"
    "virtio_scsi"
    "9p"
    "9pnet_virtio"
  ];
  boot.initrd.kernelModules = [ "nvme" "virtio_balloon" "virtio_console" "virtio_rng" ];

  boot.initrd.postDeviceCommands = lib.mkIf
    (!config.boot.initrd.systemd.enable)
    ''
      # Set the system time from the hardware clock to work around a
      # bug in qemu-kvm > 1.5.2 (where the VM clock is initialised
      # to the *boot time* of the host).
      hwclock -s
    '';
}
