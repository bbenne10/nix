{ pkgs, home-manager, userName, ...}: {
  home-manager.users.${userName} = {
    home.packages = with pkgs; [
      brightnessctl
      foot
      kanshi
      pamixer
      playerctl
      qutebrowser
      river
      spotify
      wofi
      yambar
    ];
    home.file.".config/river/init".source = ./../conf.d/river_init;
    home.file.".config/foot/foot.ini".source = ./../conf.d/foot.ini;
    home.file.".config/yambar/config.yml".source = ./../conf.d/yambar.yml;
    home.file.".config/kanshi/config".source = ./../conf.d/kanshi_config;
    home.file.".config/qutebrowser/autoconfig.yml".source = ./../conf.d/qutebrowser_autoconfig.yml;
  };
}
