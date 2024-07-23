{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-24.05";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
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

    # Darwin specific
    mac-app-util = {
      url = "github:hraban/mac-app-util";
    };

    # Linux specific
    dwl-src = {
      url = "github:bbenne10/dwl";
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
              overlays = [ emacs.overlays.package emacs.overlays.emacs ];
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
      baseLinuxModules = [
        inputs.home-manager.nixosModules.home-manager
        ./lib/nix.nix
        ./lib/common.nix
        ./lib/linux.nix
      ];
    in
    {
      nixosConfigurations = {
        "bennett-laptop" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = baseLinuxModules ++ [
            ./lib/graphical.nix
            ./hardware/laptop.nix
            ./hosts/bennett-laptop.nix
          ];
          inherit specialArgs;
        };

        "bennett-server" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = baseLinuxModules ++ [
            ./hosts/bennett-server.nix
          ];
          inherit specialArgs;
        };

        "home-server" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = baseLinuxModules ++ [
            ./hosts/home-server.nix
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
            inputs.mac-app-util.darwinModules.default
            ({ pkgs, config, ... }: {
              # Enable this for home-manager packages too
              home-manager.sharedModules = [
                inputs.mac-app-util.homeManagerModules.default
              ];
            })
            ./lib/common.nix
            ./lib/nix.nix
            ./lib/graphical.nix
            ./lib/darwin.nix
            ./hosts/cipher-4590.nix
          ];
        };
      };
      deploy.nodes = {
        server = {
          hostname = "bryan-bennett.com";
          user = "root";
          sshUser = "bryan";
          interactiveSudo = true;
          profilesOrder = [ "system" ];
          timeout = 600;
          magicRollback=false;

          profiles = {
            system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos
                self.nixosConfigurations.bennett-server;
            };
          };
        };
        home-server = {
          hostname = "home-server";
          user = "root";
          sshUser = "bryan";
          interactiveSudo = true;
          magicRollback = false;
          profiles = {
            system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos
                self.nixosConfigurations.home-server;
            };
          };
        };
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
