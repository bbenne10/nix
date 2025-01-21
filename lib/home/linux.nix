{ pkgs, ... }:
{
  home.packages = builtins.attrValues {
    inherit (pkgs) 
      bemenu
      libnotify
      xdg-utils
      xdg-desktop-portal
      wlr-randr
      wl-clipboard
      kanshi
    ;
  };

  services.kanshi = {
    enable = true;
    settings = [
      {
        profile.name = "work_docked";
        profile.outputs = [
          {
            criteria = "BOE 0x0B8E Unknown";
            mode = "1920x1080";
            scale = 1.0;
            position = "0,0";
          }
          {
            criteria = "Dell Inc. DELL U3821DW 594VZ63";
            mode = "3840x1600";
            position = "1920,0";
          }
        ];
      }
      {
        profile.name = "work_mobile";
        profile.outputs = [
          {
            criteria = "BOE 0x0B8E Unknown";
            mode = "1920x1080";
            scale = 1.0;
            position = "0,0";
          }
        ];
      }
    ];
  };

}
