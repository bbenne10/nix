{config, pkgsForSystem, ...}: {
  system.activationScripts.applications.text = pkgsForSystem.lib.mkForce (''
    rm -rf ~/Applications/Nix\ Apps
    mkdir -p ~/Applications/Nix\ Apps
    for app in $(find ${config.system.build.applications}/Applications -maxdepth 1 -type l); do
      echo "Found $app"
      src="$(/usr/bin/stat -f%Y "$app")"
      cp -r "$src" ~/Applications/Nix\ Apps
    done
  '');
}
