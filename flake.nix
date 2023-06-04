{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-23.05";
    };

    darwin = {
      url = "github:bbenne10/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      # Home-Manager hasn't yet cut their release-23.05 branch
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Server specific
    deploy-rs.url = "github:serokell/deploy-rs";

    website = {
      url = "github:bbenne10/website";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # emacs + plugins
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs_themes = {
      url = "github:bbenne10/emacs_themes";
      flake = false;
    };

    # zsh plugins
    zsh-fzf_tab = {
      url = "github:aloxaf/fzf-tab";
      flake = false;
    };

    zsh-fast_syntax_highlighting = {
      url = "github:zdharma-continuum/fast-syntax-highlighting";
      flake = false;
    };

    zsh-fzf_marks = {
      url = "github:urbainvaes/fzf-marks";
      flake = false;
    };
  };

  outputs =
    { self, nixpkgs, deploy-rs, emacs, darwin, ... }@inputs:
    let
      genAttrs = list: f: nixpkgs.lib.genAttrs list f;
      systems = [ "x86_64-darwin" "x86_64-linux" ];
      pkgsBySystem = (
        let
          mkPkgs = system:
            import nixpkgs {
              inherit system;
              overlays = [ emacs.overlay ];
              config = { allowUnfree = true; };
            };
        in
        genAttrs systems mkPkgs
      );
      darwinPkgs = pkgsBySystem.x86_64-darwin;
      linuxPkgs = pkgsBySystem.x86_64-linux;
      specialArgs = inputs // {
        userName = "bryan";
        system = "x86_64-linux";
        pkgs = linuxPkgs;
      };
    in
    {
      nixosConfigurations = {
        "bennett-laptop" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./hardware/laptop.nix
            ./lib/nix.nix
            ./lib/common.nix
            ./lib/graphical.nix
            ./lib/linux.nix
            ./hosts/bennett-laptop.nix
          ];
          inherit specialArgs;
        };

        "bennett-server" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            inputs.home-manager.nixosModules.home-manager
            ./lib/nix.nix
            ./lib/common.nix
            ./lib/linux.nix
            ./hosts/bennett-server.nix
          ];
          inherit specialArgs;
        };
      };
      darwinConfigurations = {
        "cipher-4590" = darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          specialArgs = specialArgs // {
            pkgs = darwinPkgs;
            userName = "bbennett37";
            system = "x86_64-darwin";
          };
          modules = [
            inputs.home-manager.darwinModules.home-manager
            ./lib/common.nix
            ./lib/nix.nix
            ./lib/graphical.nix
            ./lib/darwin.nix
          ];
        };
      };
      deploy.nodes = {
        server = {
          hostname = "bryan-bennett.com";
          user = "root";
          sshUser = "bryan";
          magicRollback = false;
          sshOpts = [ "-t" ];
          profilesOrder = [ "system" ];

          profiles = {
            system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos
                self.nixosConfigurations.bennett-server;
            };
          };
        };
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
