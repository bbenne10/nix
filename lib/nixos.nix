{ nixpkgs, pkgsForSystem, home-manager, userName, ...}: {
  system.stateVersion = "22.05";

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  # `nix shell nixpkgs#foo` should use our system flake
  nix.registry.nixpkgs.flake = nixpkgs;
}
