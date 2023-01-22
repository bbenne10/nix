{ pkgs, userName, ... }: {
  system.stateVersion = 4;
  services.mopidy.mediakeys.enable = true;
  services.nix-daemon.enable = true;
  system.keyboard = {
    enableKeyMapping = true;
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
