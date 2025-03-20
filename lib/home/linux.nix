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

  services.fnott = {
    enable = true;
    settings = {
      "main" = {
        border-size = 3;
        summary-font = "RecMonoSmCasual Nerd Font:size=10";
        title-font = "RecMonoSmCasual Nerd Font:size=10";
        body-font = "RecMonoSmCasual Nerd Font:size=10";
        max-timeout = 20;
        default-timeout = 5;
        idle-timeout = 20;

        progress-bar-height = 20;

        progress-bar-color = "D3C6AAFF";
        body-color = "D3C6AAFF";
        summary-color = "D3C6AAFF";
        title-color = "D3C6AAFF";
      };

      critical = {
        background = "4C3743FF";
      };

      normal = {
        background = "493B40FF";
      };

      low = {
        background = "45443CFF";
      };
    };
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
