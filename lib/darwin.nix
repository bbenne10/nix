{ pkgs, userName, ... }: {
  system.stateVersion = 4;
  users.users.${userName} = {
    home = "/Users/${userName}";
  };

  home-manager.users.${userName} = {
    home = {
      packages = with pkgs; [
        docker
      ];

      file = {
        ".gnupg/gpg-agent.conf" = {
          enable = true;
          text = ''
            pinentry-program ${pkgs.lib.getExe pkgs.pinentry_mac}
          '';
        };
      };
    };
  };

  launchd.agents."colima.default" = {
    command = "${pkgs.colima}/bin/colima start -c 4 -m 12 -d 100 --foreground";
    serviceConfig = {
      Label = "com.colima.default";
      RunAtLoad = true;
      KeepAlive = true;

      # not sure where to put these paths and not reference a hard-coded `$HOME`; `/var/log`?
      StandardOutPath = "/Users/${userName}/.colima/default/daemon/launchd.stdout.log";
      StandardErrorPath = "/Users/${userName}/.colima/default/daemon/launchd.stderr.log";

      # not using launchd.agents.<name>.path because colima needs the system ones as well
      EnvironmentVariables = {
        PATH = "${pkgs.colima}/bin:${pkgs.docker}/bin:/usr/bin:/bin:/usr/sbin:/sbin";
      };
    };
  };

  services.nix-daemon.enable = true;
  system.keyboard = {
    enableKeyMapping = true;
    # Values are 0x700000000 or'd with value from "Keyboard/Keypad Page" here:
    # https://www.usb.org/sites/default/files/hut1_4.pdf
    userKeyMapping = [
      {
        # Map F23 to F13
        HIDKeyboardModifierMappingSrc = 30064771186;
        HIDKeyboardModifierMappingDst = 30064771176;
      }
      {
        # Map CapsLock to F13
        HIDKeyboardModifierMappingSrc = 30064771129;
        HIDKeyboardModifierMappingDst = 30064771176;
      }
    ];
  };
  system.defaults = {
    dock = {
      autohide = true;
      orientation = "left";
    };
    NSGlobalDomain = {
      NSDocumentSaveNewDocumentsToCloud = false; # Do not save new documents to iCloud
      NSAutomaticPeriodSubstitutionEnabled = false; # Turn off auto-add of periods.
      NSAutomaticQuoteSubstitutionEnabled = false; # Do not substitute "smart" quotes
      NSAutomaticCapitalizationEnabled = false; # Do not autocorrect my capitalization

      "com.apple.swipescrolldirection" = false; # disable "natural" scolling
      "com.apple.mouse.tapBehavior" = 1; # enable tap-to-click
    };

  };
}
