{ pkgs
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

    programs.emacs = let package = if pkgs.stdenv.isDarwin then pkgs.emacs29 else pkgs.emacs29-pgtk; in {
      enable = true;
      package = pkgs.emacsWithPackagesFromUsePackage {
        inherit package;

        config = ./../conf.d/emacs.el;
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
      enable = true;
      package = if pkgs.stdenv.isLinux then pkgs.librewolf else null;
      profiles.default = {
        name = "default";
        isDefault = true;
        extensions = builtins.attrValues {
          inherit (pkgs.nur.repos.rycee.firefox-addons)
            bitwarden
            clearurls
            consent-o-matic
            df-youtube
            facebook-container
            localcdn
            react-devtools
            sponsorblock
            tridactyl
            ublock-origin
            unpaywall
          ;
        };
        settings = {
          autoDisableScopes = 0;
        };
      };
    };

    programs.kitty = {
      enable = true;
      theme = "Everforest Dark Hard";
      font.name = "Rec Mono Semicasual";
      shellIntegration.enableZshIntegration = true;
      settings = {
        scrollback_lines = 10000;
        enable_audio_bell = "no";
        window_padding_width = 12;
        shell = "${pkgs.zsh}/bin/zsh";
        font_size = 14;
      };
    };
  };
}
