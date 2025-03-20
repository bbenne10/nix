{pkgs, ...}: 
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
    "slack"
  ];
  home.packages = builtins.attrValues {
    vpn_up = (pkgs.callPackage ../../derivations/vpn_up.nix {});

    inherit (pkgs)
      slack
      thunderbird
      ghidra
      supersonic
    ;

    inherit (pkgs.jetbrains)
      idea-community-bin
    ;
  };
}
