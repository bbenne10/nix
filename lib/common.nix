{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    alacritty
    curl
    exa
    fd
    # (firefox.override { extraNativeMessagingHosts = [ passff-host ]; })
    git
    htop
    iosevka
    mpv
    neovim
    pass
    ripgrep
    weechat
    zsh

    # ((import (pkgs.fetchFromGitHub {
    #       owner = "numtide";
    #       repo = "devshell";
    #       rev = "17a8c6b64127a19b5136f9ce4dad17ca2934f4df";
    #       sha256 = "17a8c6b64127a19b5136f9ce4dad17ca2934f4df";
    #     }).devshell))
  ];

  programs.home-manager = {
    enable = true;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        dimensions = {
          columns = 0;
          lines = 0;
        };
        padding = {
          x = 12;
          y = 12;
        };
        dynamic_padding = false;
      };
      scrolling = {
        history = 10000;
        multiplier = 3;
      };
      font = {
        normal = { family = "JetBrains Mono"; };
        bold = { family = "JetBrains Mono"; };
        italic = { family = "JetBrains Mono"; };
        size = 10.0;
        offset = {
          x = 0;
          y = 0;
        };
        glyph_offset = {
          x = 0;
          y = 0;
        };
      };
      draw_bold_text_with_bright_colors = false;
      colors = {
        primary = {
          background = "0x2E3440";
          foreground = "0xD8DEE9";
        };
        cursor = {
          text = "0x2E3440";
          cursor = "0xD8DEE9";
        };
        normal = {
          black = "0x3B4252";
          red = "0xBF616A";
          green = "0xA3BE8C";
          yellow = "0xEBCB8B";
          blue = "0x81A1C1";
          magenta = "0xB48EAD";
          cyan = "0x88C0D0";
          white = "0xE5E9F0";
        };
        bright = {
          black = "0x4C566A";
          red = "0xBF616A";
          green = "0xA3BE8C";
          yellow = "0xEBCB8B";
          blue = "0x81A1C1";
          magenta = "0xB48EAD";
          cyan = "0x8FBCBB";
          white = "0xECEFF4";
        };
        # indexed_colors = []
      };
      bell = {
        animation = "EaseOutExpo";
        color = "0xffffff";
        duration = 0;
      };
      background_opacity = 0.75;
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    enableNixDirenvIntegration = true;
    stdlib = ''
      : ''${XDG_CACHE_HOME:=$HOME/.cache}
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
        echo "''${direnv_layout_dirs[$PWD]:=$(
          echo -n "$XDG_CACHE_HOME"/direnv/layouts/
          echo -n "$PWD" | shasum | cut -d ' ' -f 1
        )}"a
      }
    '';
  };

  programs.emacs = {
    enable = true;
  };

  programs.firefox = {
    enable = true;
    profiles = {
      bryan = {
        settings = {
          "general.smoothScroll" = false;
        };
      };
    };
  };

  programs.fzf = {
    enable = true;
    defaultCommand = "fd --type file --follow";
    defaultOptions = [ "--height 40%" "--reverse" ];
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    aliases = {
      graph = "log --graph --oneline --decorate";
      up = "!git pull --ff-only && git submodule update --init --recursive";
      wip = "commit -am WIP";
      unwip = "!git log --pretty=%B -1 | grep -iq wip && git reset HEAD~";
    };
    # delta = {
    #   enable = true;
    # };
    lfs = { enable = true; };
    signing = {
      key = "4AF3A523BC147CA2";
      signByDefault = true;
    };
    ignores = [
      "*.pyc"
      ".python-version"
      ".ropeproject"
      "auto-save-list"
      "custom.el"
      "url/"
      "#*"
      "*~"
      ".#*"
      "#*#"
      "*.log"
      ".DS_Store"
      "**/*.elc"
      ".projectile"
      ".direnv/"
      ".envrc"
    ];
    userEmail = "bbenne10@gmail.com";
    userName = "Bryan Bennett";
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      add_newline = false;
      scan_timeout = 10;
      character.symbol = "➜";
    };
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableAutosuggestions = true;
    shellAliases = {
      rmr = "rm -r";
      ls = "exa";
    };

    initExtra = ''
      setopt noclobber
      setopt chasedots
      setopt no_histverify

      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;5D" backward-word

      # Accept autosuggestions with Ctrl + Spc
      bindkey "^ " autosuggest-accept

      # vterm emacs support
      function vterm_printf() {
        printf "\e]%s\e\\" "$1"
      }

      # I have no idea where this is coming from
      # But I don't need it for now
      unset RPS1;
    '';

    plugins = [
      {
        name = "fzf-tab";
        src = pkgs.fetchFromGitHub {
          owner = "Aloxaf";
          repo = "fzf-tab";
          rev = "8584ed59107f37996b977a499ea0d536d851920d";
          sha256 = "177k0wjc2rzp1kynrrmqi0zn3my198cksc6y190sw1r4ia5bsf4k";
        };
      }
      {
        name = "fast-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zdharma";
          repo = "fast-syntax-highlighting";
          rev = "be2f385453670c18c40320a7384333f98fcd9f79";
          sha256 = "1jjhv2ag1rnj1ayrwag5mxyjbdmmf5h5yxvmlc1i44pjcqz3w4s9";
        };
      }
      {
        name = "fzf-marks";
        src = pkgs.fetchFromGitHub {
          owner = "urbainvaes";
          repo = "fzf-marks";
          rev = "5400c3a10b8f054a36dcbbf35611a1d735298034"; # 12/24/20 HEAD
          sha256 = "11klccmdxfx4xdakn1k9qw8l6bxv9qwcz0g5y69r51nvlbrhkxx1";
        };
      }
    ];
  };

}
