{ nixpkgs, pkgs, userName, ...}: {
  services.nix-daemon.enable = true;
  nix = {
    package = pkgs.nixUnstable;
    registry = {
      nixpkgs = {
        flake = nixpkgs;
      };
    };
    settings = {
        trusted-users = [ "root" userName ];
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
    };
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };
}
