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
        package = pkgs.emacs-pgtk.override {
          withNS = (pkgs.stdenv.isDarwin);
        };
        alwaysEnsure = true;
        override = epkgs: epkgs // {
          inherit bennett-themes;
          ef-themes = epkgs.ef-themes.overrideAttrs {
            version = "1.2.0";
            src = pkgs.fetchurl {
              url = "https://elpa.gnu.org/packages/ef-themes-1.2.0.tar";
              sha256 = "0uEZ35u9eA8/DQDyLlv1JWnhq8hvAGakOd2YiTzhk+g=";
            };
          };
        };
      };
    };

    home.file.".config/emacs/init.el".source = ./../conf.d/emacs.el;

    programs.firefox = {
      # TODO: nur for addons
      enable = true;
    };

  };
}
