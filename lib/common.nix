{
  pkgs,
  ...
}:
{
  config = {
    programs.zsh.enable = true;
    home-manager.backupFileExtension = "bk";

    environment = {
      systemPackages = [
        pkgs.deploy-rs
        pkgs.bashInteractive
      ];
      sessionVariables = {
        XDG_DESKTOP_DIR = "$HOME";
        XDG_DOCUMENTS_DIR = "$HOME/documents";
        XDG_DOWNLOAD_DIR = "$HOME/downloads";
        XDG_MUSIC_DIR = "$HOME/music";
        XDG_PICTURES_DIR = "$HOME/pictures";
        XDG_PUBLICSHARE_DIR = "$HOME/public";
        XDG_TEMPLATES_DIR = "$HOME/templates";
        XDG_VIDEOS_DIR = "$HOME/videos";
      };
    };

  };
}
