{
  pkgs,
  emacs_themes_src,
  ...
}:
let
  bennett-themes = (
    pkgs.callPackage ./../../derivations/emacs_themes.nix {
      inherit emacs_themes_src;
    }
  );
  modern-tab-bar = (pkgs.callPackage ./../../derivations/emacs_modern_tab_bar.nix { });
in
{
  home.packages = [
    pkgs.docker
    pkgs.pandoc
    pkgs.nerd-fonts.recursive-mono # needed for fontconfig below
    pkgs.noto-fonts # needed for fontconfig
  ];

  fonts.fontconfig.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs30-pgtk;

      config = ./../../conf.d/emacs.el;
      defaultInitFile = true;
      alwaysEnsure = true;
      override =
        epkgs:
        epkgs
        // {
          inherit bennett-themes modern-tab-bar;
        };
      extraEmacsPackages = epkgs: [ epkgs.treesit-grammars.with-all-grammars ];
    };
  };

  programs.librewolf = {
    enable = true;
    # librewolf (and firefox generally) is marked broken on macos
    # this still gets the addons and config installed there though
    package = if pkgs.stdenv.isLinux then pkgs.librewolf else null;
    profiles.default = {
      name = "default";
      isDefault = true;
      search = {
        default = "ddg";
        engines = {
          google.metadata.hidden = true;
          bing.metadata.hidden = true;
          ddg = {
            urls = [
              {
                template = "https://duckduckgo.com";
                params = [
                  {
                    name = "q";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ ];
          };
          "std - Rust" = {
            urls = [
              {
                template = "https://std.rs/{searchTerms}";
                params = [ ];
              }
            ];
            definedAliases = [ "!rust" ];
          };
          "Docs.rs" = {
            urls = [
              {
                template = "https://docs.rs/releases/search";
                params = [
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ "!rdoc" ];
          };
          "Lib.rs" = {
            urls = [
              {
                template = "https://lib.rs/search";
                params = [
                  {
                    name = "q";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ "!crates" ];
          };
          "NixOS Packages" = {
            urls = [
              {
                template = "https://search.nixos.org/packages";
                params = [
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                  {
                    name = "channel";
                    value = "unstable";
                  }
                  {
                    name = "type";
                    value = "packages";
                  }
                ];
              }
            ];
            definedAliases = [ "!np" ];
          };
          "NixOS Options" = {
            urls = [
              {
                template = "https://search.nixos.org/options";
                params = [
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                  {
                    name = "channel";
                    value = "unstable";
                  }
                  {
                    name = "type";
                    value = "packages";
                  }
                ];
              }
            ];
            definedAliases = [ "!no" ];
          };
          "Noogle.dev" = {
            urls = [
              {
                template = "https://noogle.dev/q";
                params = [
                  {
                    name = "term";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [
              "!ng"
              "!noogle"
            ];
          };
          "Home Manager Options" = {
            urls = [
              {
                template = "https://home-manager-options.extranix.com";
                params = [
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                ];
              }
            ];
            definedAliases = [ "!hm" ];
          };
        };
      };
      extensions.packages = builtins.attrValues {
        inherit (pkgs.nur.repos.rycee.firefox-addons)
          bitwarden
          clearurls
          consent-o-matic
          facebook-container
          foxyproxy-standard
          localcdn
          onepassword-password-manager
          react-devtools
          remove-youtube-s-suggestions
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

  programs.alacritty = {
    enable = true;
    theme = "everforest_dark";
    settings = {
      window = {
        decorations = "None";
        padding = {
          x = 12;
          y = 12;
        };
        opacity = 0.8;
        blur = true;
      };
      scrolling = {
        history = 10000;
      };
      font = {
        normal = {
          family = "RecMonoSmCasual Nerd Font";
        };
        size = 12;
      };
      terminal = {
        shell = "${pkgs.zsh}/bin/zsh";
      };
    };
  };

  programs.mpv = {
    enable = true;
    package = (
      pkgs.mpv-unwrapped.wrapper {
        scripts = with pkgs.mpvScripts; [
          uosc
          mpris
        ];
        mpv = pkgs.mpv-unwrapped.override { waylandSupport = true; };
      }
    );
    config = {
      profile = "high-quality";
      ytdl-format = "bestvideo+bestaudio";
    };
  };
}
