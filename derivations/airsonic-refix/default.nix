# This is upstream @ 0c54a51b0ee51abaf14b472e9a22415fcbbe6fbc
# (https://github.com/tamland/airsonic-refix)
# Mirrored because they don't do releases
# and I cba to actually figure out their toolchain + nix
{
  pkgs,
}:
pkgs.stdenv.mkDerivation {
  name = "airsonic-refix";
  version = "0c54a51b0";

  src = ./.;

  dontBuild = true;
  installPhase = ''
    mkdir -p $out
    cp -r . $out
  '';
}
