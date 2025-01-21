{pkgs, ...}: 
{
  services.nix-daemon.enable = true;
  home.packages = [
    pkgs.jetbrains.idea-community-bin
  ];
}
