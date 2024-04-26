{ deploy-rs
, pkgs
, system
, userName
, zsh-fast_syntax_highlighting
, zsh-fzf_marks
, zsh-fzf_tab
, ...
}:
let
  themesh = (pkgs.callPackage ../derivations/themesh.nix { });
  darkmode = (pkgs.callPackage ../derivations/darkmode.nix { });
  deploy-rs-bin = deploy-rs.packages.${system}.deploy-rs;
in
{
  environment.systemPackages = with pkgs; [
    bashInteractive
    cachix
    deploy-rs-bin
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.zsh.enable = true;

  home-manager.users.${userName} = {
    home.enableNixpkgsReleaseCheck = true;
    home.stateVersion = "22.05";
    home.packages = with pkgs; [
      curl
      darkmode
      dtach
      dvtm
      fd
      gawk
      gnupg
      manix
      nil
      nix-output-monitor
      nix-tree
      nixfmt-classic
      openssh
      ripgrep
      rsync
      themesh
      tree
    ];

    programs.bat = {
      enable = true;
    };

    programs.htop = {
      enable = true;
    };

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
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
        wip = "commit --no-sign -am WIP --no-verify";
        unwip = "!git log --pretty=%B -1 | grep -iq wip && git reset HEAD~";
      };
      lfs = { enable = true; };
      delta = { enable = true; };
      signing = {
        key = "EE149E4215408DE9";
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
        ".direnv/"
        ".envrc"
      ];
      userEmail = "Bryan.Bennett@proton.me";
      userName = "Bryan Bennett";
      extraConfig = {
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
      };
    };

    programs.gpg = {
      enable = true;
      mutableKeys = false;
      mutableTrust = false;
      scdaemonSettings = {
        disable-ccid = true;
      };
      publicKeys = [
        {
          # yubikey 5
          text = ''
            -----BEGIN PGP PUBLIC KEY BLOCK-----

            mDMEZVy2/RYJKwYBBAHaRw8BAQdAFneOh6/zyVrvAJAocXI2caoUi96dQhm2e/NK
            MnKpe7a0LUJyeWFuIEJlbm5ldHQgKFlLNSkgPEJyeWFuLkJlbm5ldHRAcHJvdG9u
            Lm1lPoiOBBMWCgA2FiEETqGbx3D6YpkNmJpi7hSeQhVAjekFAmVctv0CGwMECwkI
            BwQVCgkIBRYCAwEAAh4FAheAAAoJEO4UnkIVQI3piX0A/2/qlK+AXNwlFYP6dJER
            JXNLyCYM6bGG9vBFlI/srht+AQCREA3BzmMfmetfqizOclTVoW2Po5p6PKaUILdV
            R2h7Crg4BGVctv0SCisGAQQBl1UBBQEBB0Aun+ARMYJOzUh7thx5gc8RxqItZXn0
            RqlHHsB9qUQ0dQMBCAeIeAQYFgoAIBYhBE6hm8dw+mKZDZiaYu4UnkIVQI3pBQJl
            XLb9AhsMAAoJEO4UnkIVQI3pi5IBALDDr9GkrWS7nDoro3O7eG+JqGWVINm7ta8I
            TdC0HjipAQDLEN2ufgeS/1Fdr1PYDPw/mP0ouxF+w4/OsE4L1xbDC7gzBGVctz8W
            CSsGAQQB2kcPAQEHQN05GNTpS1NpA9W52N1tNKlS8yuRzRwTqwpL25CA7CCDiHgE
            GBYKACAWIQROoZvHcPpimQ2YmmLuFJ5CFUCN6QUCZVy3PwIbIAAKCRDuFJ5CFUCN
            6UCRAQDGMbm1CtlobTFJ69Qf0a1nQ1GqwU9tYSn6snXycE8SkgEAvtOAFD6g2RuZ
            FPMNQmarY2X4fi6eDPdAjzZAhT69uwU=
            =Yp2h
            -----END PGP PUBLIC KEY BLOCK-----
          '';
          trust = "ultimate";
        }
      ];
    };

    programs.ssh = {
      enable = true;
      forwardAgent = true;
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

      initExtra = ''
        theme.sh nord
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
