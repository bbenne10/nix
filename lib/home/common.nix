{
  pkgs,
  specialArgs,
  ...
}:
{
  home.stateVersion = "22.05";

  home.packages = builtins.attrValues {
    inherit (pkgs)
      curl
      dtach
      dvtm
      fd
      gawk
      gh
      jq
      nil
      nix-output-monitor
      nix-tree
      nixfmt-rfc-style
      openssh
      ripgrep
      rsync
      tree
      ;
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "ansi";
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    stdlib = ''
      # Centralize direnv layouts in $HOME/.cache/direnv/layouts
      : "''${XDG_CACHE_HOME:="''${HOME}/.cache"}"
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
          local hash path
          echo "''${direnv_layout_dirs[$PWD]:=$(
            hash="$(sha1sum - <<< "$PWD" | head -c40)"
            path="''${PWD//[^a-zA-Z0-9]/-}"
            echo "''${XDG_CACHE_HOME}/direnv/layouts/''${hash}''${path}"
          )}"
       }
    '';
  };

  programs.fzf = {
    enable = true;
    defaultCommand = "fd --type file --follow";
    defaultOptions = [
      "--height 40%"
      "--reverse"
    ];
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    aliases = {
      graph = "log --graph --oneline --decorate";
      up = "!git pull --ff-only && git submodule update --init --recursive";
      wip = "commit --no-sign -am WIP --no-verify";
      unwip = "!git log --pretty=%B -1 | grep -iq wip && git reset HEAD~";
    };
    lfs = {
      enable = true;
    };
    delta = {
      enable = true;
    };
    # signing = {
    #   key = "5DDFE2C35409EFE83F70C32788393D9EC269636D";
    #   signByDefault = true;
    # };
    ignores = [
      "*.pyc"
      ".python-version"
      ".ropeproject"
      "auto-save-list"
      "custom.el"
      "url/"
      "*~"
      ".\#*"
      "\#*\#"
      "*.log"
      ".DS_Store"
      "**/*.elc"
      ".direnv/"
      ".envrc"
    ];
    userEmail = "Bryan.Bennett@proton.me";
    userName = "Bryan Bennett";
    extraConfig = {
      core.fsmonitor = true;
      init = {
        defaultBranch = "main";
      };
      pull = {
        rebase = true;
      };
      push = {
        default = "current";
        autoSetupRemote = "true";
      };
      "gitlab.gitlab.gtri.gatech.edu/api/v4".User = "bbennett37";
    };
  };

  programs.gpg = {
    enable = true;
    mutableKeys = true;
    mutableTrust = true;
    scdaemonSettings = {
      disable-ccid = true;
    };
    # publicKeys = [
    #   {
    #     # yubikey 5
    #     source = ./../../conf.d/pub.key;
    #     trust = "ultimate";
    #   }
    # ];
  };

  programs.helix = {
    enable = true;
    settings = {
      theme = "everforest_dark";
      editor = {
        statusline = {
          mode = {
            normal = "N";
            insert = "I";
            select = "S";
          };
        };
        auto-pairs = false;
      };
    };
  };

  programs.htop = {
    enable = true;
  };

  programs.home-manager.enable = true;

  programs.ssh = {
    enable = true;
    forwardAgent = true;
    matchBlocks = {
      "*.malnet" = {
        proxyJump = "malnet-jump.apiary.gtri.gatech.edu:222";
      };
      "ciph-*.malnet" = {
        user = "admin";
        identityFile = [ "~/.ssh/id_apiary_admin" ];
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
    autosuggestion.enable = true;
    shellAliases = {
      rmr = "rm -r";
      ls = "${pkgs.eza}/bin/eza";
      cat = "${pkgs.bat}/bin/bat";
      gpgreset = "gpg-connect-agent killagent /bye; gpg-connect-agent updatestartuptty /bye; gpg-connect-agent /bye";
    };

    initContent = ''
      setopt noclobber
      setopt chasedots
      setopt no_histverify

      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;5D" backward-word

      # Accept autosuggestions with Ctrl + Spc
      bindkey "^ " autosuggest-accept

      function set_title () {
        echo -en "\033]1; $@ \007"
      }

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
        src = specialArgs.zsh-fzf_tab;
      }
      {
        name = "fast-syntax-highlighting";
        src = specialArgs.zsh-fast_syntax_highlighting;
      }
      {
        name = "fzf-marks";
        src = specialArgs.zsh-fzf_marks;
      }
    ];
  };
}
