{ pkgs, ... }:
{
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (pkgs.lib.getName pkg) [
      "slack"
      "1password"
      "onepassword-password-manager"
    ];

  programs.librewolf.profiles.default.extensions.packages = [ pkgs.nur.repos.rycee.firefox-addons.onepassword-password-manager ];

  home.packages = builtins.attrValues {
    vpn_up = (pkgs.callPackage ../../derivations/vpn_up.nix { });

    inherit (pkgs)
      slack
      thunderbird
      ghidra
      supersonic
      _1password-gui
      ;

    inherit (pkgs.jetbrains)
      idea-community-bin
      ;
  };

  };
}
