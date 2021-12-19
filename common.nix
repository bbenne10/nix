{ config, lib, pkgsForSystem, home-manager, userName, zsh-fzf_tab, zsh-fast_syntax_highlighting, zsh-fzf_marks, ...}: {
  users.users.${userName}.home = if pkgsForSystem.stdenv.isDarwin then "/Users/${userName}" else "/home/${userName}";
  nix.trustedUsers = [ userName ];
  nix.package = pkgsForSystem.nixUnstable;
  services.nix-daemon.enable = true;
  fonts = {
    enableFontDir = true;
    fonts = with pkgsForSystem; [
      shareTechMono
      noto-fonts
      noto-fonts-emoji
      fira-code
      fira-code-symbols
      hasklig
    ];
  };
  programs.zsh.enable = true;
  system.stateVersion = 4;

  home-manager.users.${userName} = {
    home.stateVersion = "20.09";
    home.packages = with pkgsForSystem; [
      curl
      exa
      fd
      git
      gnupg
      htop
      manix
      pandoc
      pass
      ripgrep
      weechat
    ];

    programs.emacs = {
      enable = true;
      package = pkgsForSystem.emacsWithPackagesFromUsePackage {
        config = ./conf.d/emacs.el;
        package = pkgsForSystem.emacs;
        alwaysEnsure = true;
      };
    };

    home.file.".config/emacs/init.el".source = ./conf.d/emacs.el;

    programs.kitty = {
      enable = true;
      settings = {
        shell = "${pkgsForSystem.zsh}/bin/zsh";
        font_family = "Share Tech Mono";
        font_size = "14.0";
        font_features = "ShareTechMono-Regular -liga";
        macos_option_as_alt = true;
        macos_custom_beam_cursor = true;
        background = "#2E3440";
        foreground = "#D8DEE9";
        color0 = "#3B4252";
        color1 = "#BF616A";
        color2 = "#A3BE8C";
        color3 = "#EBCB8B";
        color4 = "#81A1C1";
        color5 = "#B48EAD";
        color6 = "#88C0D0";
        color7 = "#E5E9F0";
        color8 = "#4C566A";
        color9 = "#BF616A";
        color10 = "#A3BE8C";
        color11 = "#EBCB8C";
        color12 = "#81A1C1";
        color13 = "#B48EAD";
        color14 = "#8FBCBB";
        color15 = "#ECEFF4";
      };
    };

    programs.bat = {
      enable = true;
    };

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = {
        enable = true;
      };
      stdlib = ''
        # Centralize direnv layouts in $HOME/.cache/direnv/layouts
        : ''${XDG_CACHE_HOME:=$HOME/.cache}
        declare -A direnv_layout_dirs
        direnv_layout_dir() {
          echo "''${direnv_layout_dirs[$PWD]:=$(
            echo -n "$XDG_CACHE_HOME"/direnv/layouts/
            echo -n "$PWD" | shasum | cut -d ' ' -f 1
                                             )}"
        }
      '';
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
        wip = "commit --no-sign -am WIP";
        unwip = "!git log --pretty=%B -1 | grep -iq wip && git reset HEAD~";
      };
      lfs = { enable = true; };
      signing = {
        key = "41EA00B400F969701CB2D3AFEF90E3E98B8F5C0B";
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
      userEmail = "Bryan.Bennett@protonmail.com";
      userName = "Bryan Bennett";
      extraConfig = {
        pull = {
          rebase = true;
        };
      };
    };

    programs.htop = {
      enable = true;
      settings = {
        show_program_path = true;
      };
    };

    programs.ssh = {
      enable = true;
      controlMaster = "yes";
      controlPersist = "10m";
      forwardAgent = true;
      matchBlocks = {
        "desktop" = {
          hostname = "tia-bxb-d02";
          user = "bbennett37";

        };
      };
    };

    programs.starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        add_newline = false;
        scan_timeout = 10;
        character.success_symbol = "[➜](bold green)";
        character.error_symbol = "[➜](bold red)";
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
        cat = "bat";
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

        # ^Z to foreground the last suspended job
        foreground-current-job() { fg; }
        zle -N foreground-current-job
        bindkey -M emacs '^z' foreground-current-job

        gpg-connect-agent /bye
      '';

      envExtra = ''
        export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      '';

      plugins = [
        {
          name = "fzf-tab";
          src = zsh-fzf_tab;
        }
        {
          name = "fast-syntax-highlighting";
          src = zsh-fast_syntax_highlighting;
        }
        {
          name = "fzf-marks";
          src = zsh-fzf_marks;
        }
      ];
    };
  };
}
