{ config
, pkgs
, home-manager
, userName
, ...
}:
let
  bennett-themes = (pkgs.callPackage ../derivations/emacs_themes.nix { });
in
{
  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "ShareTechMono" ]; })
      noto-fonts
      recursive
    ];
  };

  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [
      mopidy-ytmusic
      mopidy-iris
      mopidy-somafm
      mopidy-bandcamp
      mopidy-soundcloud
    ];
  };

  home-manager.users.${userName} = {
    home.enableNixpkgsReleaseCheck = true;
    home.stateVersion = "22.05";
    home.packages = with pkgs; [
      colima
      docker
      pandoc
    ];

    programs.emacs = {
      enable = true;
      package = pkgs.emacsWithPackagesFromUsePackage {
        config = ./../conf.d/emacs.el;
        package = pkgs.emacsPgtk;
        alwaysEnsure = true;
        override = epkgs: epkgs // {
          inherit bennett-themes;
        };
      };
    };

    home.file.".config/emacs/init.el".source = ./../conf.d/emacs.el;
  };
}
