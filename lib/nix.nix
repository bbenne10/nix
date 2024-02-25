{ lib, nixpkgs, pkgs, userName, system, ... }: {
  nix = {
    package = pkgs.nixUnstable;
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
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
      ];
      trusted-users = [ "root" userName ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    optimise = {
      automatic = true;
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
      persistent = true;
    };
  };
  system.activationScripts.diff = {
    supportsDryActivation = true;
    text = ''
      ${pkgs.nvd}/bin/nvd --nix-bin-dir=${pkgs.nix}/bin diff /run/current-system "$systemConfig"
    '';
  };
}
