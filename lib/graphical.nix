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
    packages = with pkgs; [
      (nerdfonts.override { fonts = [ "ShareTechMono" ]; })
      noto-fonts
      recursive
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
        package = pkgs.emacs29-pgtk;
        alwaysEnsure = true;
        override = epkgs: epkgs // {
          inherit bennett-themes;
        };
        extraEmacsPackages = (epkgs: with epkgs; [
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
