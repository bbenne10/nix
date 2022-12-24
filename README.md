# Nix configuration for a bunch of machines

## Deploying

First: Clone the repo

### Darwin

``` console
$ cd <repo clone root>
$ nix build ~/.config/darwin\#darwinConfigurations.cipher-4590
$ ./result/sw/bin/darwin-rebuild switch --flake .
```

After completing the above, 
`darwin-rebuild` will be in `$PATH`,
so a simple `darwin-rebuild switch --flake <path>` will work fine.o

### NixOS

```console
$ nixos-rebuild switch --flake <repo clone root>
```

will work
(if your hostname happens to be `bennett-{server,laptop}`).
