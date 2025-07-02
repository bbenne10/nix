{ pkgs, ... }:
{
  fonts = {
    packages = [
      pkgs.nerd-fonts.recursive-mono
      pkgs.noto-fonts
    ];
  };
}
