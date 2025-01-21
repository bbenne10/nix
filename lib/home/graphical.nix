{ pkgs
, emacs_themes_src
, ...
}: let
  bennett-themes =
    (pkgs.callPackage ./../../derivations/emacs_themes.nix {
      inherit emacs_themes_src;
    });
in
{
  home.packages = [
    pkgs.docker
    pkgs.pandoc
    (pkgs.nerdfonts.override { fonts = ["Recursive" "Noto"]; })
  ];

  fonts.fontconfig.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs30-pgtk;

      config = ./../../conf.d/emacs.el;
      defaultInitFile = true;
      alwaysEnsure = true;
      override = epkgs: epkgs // {
        inherit bennett-themes;
      };
      extraEmacsPackages = (epkgs: [
        epkgs.treesit-grammars.with-all-grammars
      ]);
    };
  };

  programs.firefox = {
    enable = true;
    package = if pkgs.stdenv.isLinux then pkgs.firefox else null;
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
    themeFile = "everforest_dark_hard";
    font.name = "RecMonoSmCasual Nerd Font";
    font.size = 12;
    shellIntegration.enableZshIntegration = true;
    keybindings = {
      "f13>d" = "layout_action decrease_num_full_size_windows";
      "f13>i" = "layout_action increase_num_full_size_windows";
      "f13>/" = "layout_action mirror toggle";
      "f13>PLUS" = "layout_action mirror bias 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75";
      "f13>j" = "next_window";
      "f13>k" = "previous_window";
      "f13>ENTER" = "new_window_with_cwd";
      "f13>J" = "next_tab";
      "f13>K" = "prev_tab";
    };
    settings = {
      scrollback_lines = 10000;
      enable_audio_bell = "no";
      window_padding_width = 12;
      shell = "${pkgs.zsh}/bin/zsh";
      enabled_layouts = "tall:bias=50;full_size=1;mirrored=false";
      background_opacity = 0.9;
    };
  };
}
