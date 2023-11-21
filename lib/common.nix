{ config
, deploy-rs
, lib
, pkgs
, home-manager
, nix-index-database
, system
, userName
, environment
, zsh-fzf_tab
, zsh-fast_syntax_highlighting
, zsh-fzf_marks
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
    nixUnstable
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
      bitwarden-cli
      curl
      nix-index-database.packages.${pkgs.stdenv.system}.comma-with-db
      darkmode
      dtach
      dvtm
      fd
      gawk
      gnupg
      manix
      mpv
      nix-tree
      nixUnstable
      nixfmt
      openssh
      ripgrep
      rnix-lsp
      rsync
      themesh
      tree
    ];

    programs.bat = {
      enable = true;
    };

    programs.btop = {
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
        wip = "commit --no-sign -am WIP --no-verify";
        unwip = "!git log --pretty=%B -1 | grep -iq wip && git reset HEAD~";
      };
      lfs = { enable = true; };
      delta = { enable = true; };
      signing = {
        key = "EE149E4215408DE9"; # 41EA00B400F969701CB2D3AFEF90E3E98B8F5C0B";
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
      mutableKeys = true;
      mutableTrust = true;
      scdaemonSettings = { disable-ccid = true; };
      publicKeys = [
        {
          # yubikey 4
          text = ''
            -----BEGIN PGP PUBLIC KEY BLOCK-----

            mQENBGG435gBCAC805OTSnHrC7ciKpsy4SDwMrJRCvdqbd6JvkJKrhe09llqcVY7
            FltPPwetiTXdjDN2+v4AT2LWM9jElRRVi6IOdbQofnHVGO8JDmr6Cd8YugmAn2W6
            pAqR+1UNF2s6hUqZJNZasIxxrqXeGCFL/eb/t/Fj8aGTKA38ydJMKAB0o93UcbGo
            x1Mi7wFfNHify+xEUiYcTbtHug0ucwXWSVBT13KnQ1AagXsKu6Af4+V1bdCPvG5q
            PavY0oRN5mks1B8siwv50VrRuCb96rcNU4HlRmomi2hcZaEKpP2zKYzXjGH5cjJe
            KpBN1HiCYZ3iF88wBPHEPV7JTx2Jbq0QaooLABEBAAG0LEJyeWFuIEJlbm5ldHQg
            PEJyeWFuLkJlbm5ldHRAcHJvdG9ubWFpbC5jb20+iQFOBBMBCAA4FiEEQeoAtAD5
            aXAcstOv75Dj6YuPXAsFAmG435gCGw8FCwkIBwIGFQoJCAsCBBYCAwECHgECF4AA
            CgkQ75Dj6YuPXAvl3wf8D8ZYjMNyQO1vtM+vubEy9VX9MI8Y+TJUrx7FS7ynpIG5
            oU4x7mQfW2DpRMG0IALUqF3oVSe1oY5WrZGTnDfjFcmDGGGFm+vgPJQloRc0JTQS
            7vFVtMZqxX5CRagtLm8JyXU5rO5KGRqrwAHf0+cD+WPt61PEvt39cU+aVHnZ55uv
            szonV2InWCLzcN5ukMVEptoCDiLyk5PDK59tO364UmkGSeMIleORw4TrauINfmd1
            6P8iNw9Y4IMgiOc964pVM/gfAEIraZiTN2RNFs44JzKERVHt0dUco/ZCtuceWjs/
            o4RIRKscIEq5AlJr2Z7zfea8fFNv/zXsgb5LOQjNjLkBDQRhuN/FAQgA0LZnQOb4
            aDXDdeD3hUvNGPvgVT0gSSmo4NYZana+XcIa/lIOs71B1aSct+hWbzOlS9Lf3AeB
            sBab/QfrR5BAk3x0IEC70NaUCLmuQchH94YXOaMFVgN1jkGvRPdhfjxUfwIzSR2k
            KkgGXr2KG8UU9N6+HwpMGjg11rrbc9N9eIVdByTGT12UMR6zxKQQZpuwHHrVrLuM
            xMQsMBDgytWk+1rw1KegryEGgKkUpH2UIYqw//xO14AoU71XFW41VdhOmpEjj69H
            AGW9PD+v1413pnXVd6ISbYMxlQfM4sQn9+Ged3rQ+I+jGlVo4kZr+Sdj333Oys8M
            HY7PixZEq99vCwARAQABiQE2BBgBCAAgFiEEQeoAtAD5aXAcstOv75Dj6YuPXAsF
            AmG438UCGyAACgkQ75Dj6YuPXAvI/wf/RVjCF9zEw/Xk9rKVG74Mr1MpZbGVT8XQ
            uXJ0WzZrcfRDkhX5+3+DkF4T4RlvEZKfVsYu5rrjMenGuT9wrX8i0GRxUC8pQ7er
            aH/gwmh4ohbyqCyz/VisSasq3EeUFlR9OtiIj9BmkI+0aUgOue17ig3LBHduMvu/
            rlvVkzxFgsRFDcRrZXkOnBX0gJVl7YjY61NBx9Y0AEAfzO34CsuPrf3kt12IaVV8
            11yglK7c0LtTouKQZxHdky+b5I3JuS28RhfeJ6eWIPc+5p+90Daf8mA92QKMXDvr
            T4NpJ2/hTmCr49+VOabR0yv1iDjHagZywgT+fTxkwzonAlYXFXyMzw==
            =1nQI
            -----END PGP PUBLIC KEY BLOCK-----'';
          trust = "ultimate";
        }
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
            -----END PGP PUBLIC KEY BLOCK-----'';
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
      enableAutosuggestions = true;
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
