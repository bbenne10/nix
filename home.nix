{ config, pkgs, ... }:
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = "bryan";
  home.homeDirectory = "/home/bryan";
  home.stateVersion = "20.09";

  nixpkgs.config.allowUnfree = true;
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
  #   }))
  # ];

  xsession = {
    enable = true;
    windowManager = {
      command = "${pkgs.dwm}/bin/dwm";
    };
    initExtra = ''
      . ~/.fehbg;
    '';
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 48; 
    };
  };

  xresources.properties = {
      "Xft.dpi" =  "192";
  };

  home.packages = with pkgs; [
    acpitool
    alacritty
    brightnessctl
    curl
    dwm
    # (emacsWithPackagesFromUsePackage {
    #   config = /home/bryan/code/dotfiles/emacs.org;
    #   package = pkgs.emacsUnstable;
    #   alwaysEnsure = true;
    #   })
    exa
    fd
    feh
    firefox
    gcc
    git
    gnum4
    gnumake
    htop
    i3lock
    jetbrains-mono
    pass
    pciutils
    picom
    pmutils
    pulseaudio-ctl
    ripgrep
    rofi
    rofi-pass
    vim
    weechat
    xss-lock
    zsh
  ];

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
        normal = {
	        family = "JetBrains Mono";
        };
	      bold = {
	        family = "JetBrains Mono";
        };
	      italic = {
	        family = "JetBrains Mono";
        };
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
      visual_bell = {
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
    lfs = {
      enable = true;
    };
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
      "\#*"
      "*~"
      ".#*"
      "\#*\#"
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
    
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  programs.opam = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
        add_newline = false;
        # prompt_order = [ "package" "character" ];
        scan_timeout = 10;
        character.symbol = "➜";
      };
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    shellAliases = {
      rmr = "rm -r";
    };
    initExtra = ''
      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;5D" backward-word

      # Accept autosuggestions with Ctrl + Spc
      bindkey "^ " autosuggest-accept
    '';
  };


  # services
  services.picom = {
    enable = true;
    experimentalBackends = true;
    fade = true;
    vSync = true;
    blur = true;
  };
}
