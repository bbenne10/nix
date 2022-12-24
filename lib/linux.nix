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
  users.users.${userName} = {
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQtmdA5vhoNcN14PeFS80Y++BVPSBJKajg1hlqdr5dwhr+Ug6zvUHVpJy36FZvM6VL0t/cB4GwFpv9B+tHkECTfHQgQLvQ1pQIua5ByEf3hhc5owVWA3WOQa9E92F+PFR/AjNJHaQqSAZevYobxRT03r4fCkwaODXWuttz0314hV0HJMZPXZQxHrPEpBBmm7AcetWsu4zExCwwEODK1aT7WvDUp6CvIQaAqRSkfZQhirD//E7XgChTvVcVbjVV2E6akSOPr0cAZb08P6/XjXemddV3ohJtgzGVB8zixCf34Z53etD4j6MaVWjiRmv5J2Pffc7Kzwwdjs+LFkSr328L cardno:000606534762"
    ];
  };
}
