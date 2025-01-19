{ userName, pkgs, ... }:
{
  services.nix-daemon.enable = true;
  home-manager.users.${userName}.home.packages = builtins.attrValues {
    inherit (pkgs.jetbrains) idea-community-bin;
  };
}
