{ config
, pkgs
, home-manager
, userName
, emacs_themes
, ...
}:
let
  bennett-themes =
    (pkgs.callPackage ../derivations/emacs_themes.nix {
      inherit emacs_themes;
    });
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
    enable = false;
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
        package = pkgs.emacs29;
        alwaysEnsure = true;
        override = epkgs: epkgs // {
          inherit bennett-themes;
          mood-line = epkgs.mood-line.overrideAttrs
            (_: prev_attrs: {
              src = pkgs.fetchFromGitLab
                {
                  owner = "jessieh";
                  repo = "mood-line";
                  rev = "a15d166249f04b047a6136856e5be109357762d3";
                  hash = "sha256-Y9n0p3jO5Ol/uUigrRNfrfxD5aeeb98NjNSDtroRffc=";
                };
            });
        };
        extraEmacsPackages = (epkgs: with epkgs;
          [
            treesit-grammars.with-all-grammars
          ]);
      };
    };

    home.file.".config/emacs/init.el".source = ./../conf.d/emacs.el;

    programs.firefox = {
      # TODO: nur for addons
      enable = (pkgs.stdenv.isLinux);
    };

  };
}
