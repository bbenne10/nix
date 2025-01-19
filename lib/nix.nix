{ lib, nixpkgs, pkgs, userName, system, ... }: {
  nix = {
    channel.enable = false;
    registry = {
      nixpkgs = {
        flake = nixpkgs;
      };
    };
    settings = {
      max-jobs = lib.mkDefault 8;
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://emacs.cachix.org"
        "https://cache.lix.systems"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
      ];
      trusted-users = [ "root" userName ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };
  # system.activationScripts.diff = {
  #   supportsDryActivation = true;
  #   text = ''
  #     ${pkgs.nvd}/bin/nvd --nix-bin-dir=${pkgs.nix}/bin diff /run/current-system "$systemConfig"
  #   '';
  # };
}
