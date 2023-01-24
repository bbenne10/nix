{ config
, pkgs
, home-manager
, userName
, emacs_themes
, ...
}:
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
          bennett-themes = epkgs.trivialBuild {
            pname = "bennett-themes";
            src = emacs_themes;
            postBuild = ''
              emacs -L . --batch -f batch-byte-compile themes/*.el
            '';
            postInstall = ''
              install themes/*.el themes/*.elc $out/share/emacs/site-lisp
            '';
          };
        };
      };
    };

    home.file.".config/emacs/init.el".source = ./../conf.d/emacs.el;
  };
}
