{ userName, pkgs, ... }:
let
  acsaml = pkgs.callPackage ../derivations/acsaml.nix { };
in
{
  services.nix-daemon.enable = true;

  home-manager.users.${userName} = {
    home.packages = [ acsaml ];
  };
}
