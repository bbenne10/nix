{ sysPkgs
, userName
, emacs_themes
, ...
}:
let
  bennett-themes =
    (sysPkgs.callPackage ../derivations/emacs_themes.nix {
      inherit emacs_themes;
    });
in
{
  fonts = {
    packages = builtins.attrValues {
      inherit (sysPkgs)
      noto-fonts
      recursive
    ;
    };
  };

  home-manager.users.${userName} = {
    home.enableNixpkgsReleaseCheck = true;
    home.stateVersion = "22.05";
    home.packages = builtins.attrValues {
      inherit (sysPkgs)
      colima
      docker
      pandoc
      ;
    };

    programs.emacs = let package = if sysPkgs.stdenv.isDarwin then sysPkgs.emacs30 else sysPkgs.emacs30-pgtk; in {
      enable = true;
      package = sysPkgs.emacsWithPackagesFromUsePackage {
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
      package = if sysPkgs.stdenv.isLinux then sysPkgs.librewolf else null;
      profiles.default = {
        name = "default";
        isDefault = true;
        extensions = builtins.attrValues {
          inherit (sysPkgs.nur.repos.rycee.firefox-addons)
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
      themeFile = "everforest_dark_hard";
      font.name = "Rec Mono Semicasual";
      shellIntegration.enableZshIntegration = true;
      keybindings = {
        "f13>d" = "layout_action decrease_num_full_size_windows";
        "f13>i" = "layout_action increase_num_full_size_windows";
        "f13>/" = "layout_action mirror toggle";
        "f13>+" = "layout_action mirror bias 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75";
        "f13>j" = "next_window";
        "f13>k" = "previous_window";
        "f13>ENTER" = "new_window_with_cwd";
        "f13>J" = "next_tab";
        "f13>K" = "prev_tab";
      };
      settings = {
        enable_audio_bell = "no";
        enabled_layouts = "tall:bias=50;full_size=1;mirrored=false";
        font_size = if sysPkgs.stdenv.isDarwin then 14 else 10;
        scrollback_lines = 10000;
        shell = "${sysPkgs.zsh}/bin/zsh";
        window_padding_width = 12;
        background_opacity = 0.8;
      };
    };
  };
}
