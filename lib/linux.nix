{ pkgs, home-manager, userName, ...}: {
  home-manager.users.${userName} = {
    home.packages = with pkgs; [
      brightnessctl
      firefox
      foot
      pamixer
      playerctl
      river
      spotify
      wofi
      yambar
    ];
    home.file.".config/river/init".source = ./../conf.d/river_init;
    home.file.".config/foot/foot.ini".source = ./../conf.d/foot.ini;
    home.file.".config/yambar/config.yml".source = ./../conf.d/yambar.yml;
  };
}
