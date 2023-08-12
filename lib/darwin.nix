{ pkgs, userName, ... }: {
  system.stateVersion = 4;
  users.users.${userName} = {
    home = "/Users/${userName}";
  };

  home-manager.users.${userName} = {
    home = {
      packages = with pkgs; [
        colima
        iterm2
      ];
    };
  };

  services.mopidy.mediakeys.enable = true;
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
