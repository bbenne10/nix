# {writeShellScriptBin, writeText, beets, beetsLibraryDir, }:
{config, lib, pkgs}:
let
  cfg = config.services.autobeets;
in {
  options = {
    services.autobeets = {
      enable = lib.mkEnableOption "Enable Autobeets";
      user = {
        type = lib.types.str;
      };
      interval = {
        type = lib.types.str;
        default = "30m";
      };
      directories = {
        destination = lib.mkOption {
          type = lib.types.str;
        };
        source = lib.mkOption {
          type = lib.types.str;
        };
      };
    };
  };
  config = let
    beet = "${pkgs.beets}/bin/beet";
    beetCfg = lib.mkIf cfg.enable pkgs.writeText "config.yaml" builtins.toJSON {
      directory = cfg.directories.destination;
      musicbrainz = {
        genres = "yes";
      };
    };
    autobeets = lib.mkIf cfg.enable pkgs pkgs.writeShellScriptBin
      "autobeets"
      ''
        beet -vv -c "${beetCfg}" import "${cfg.directories.source}" -q
      ''; in {
        systemd.services.autobeets = {
          description = "Auto beets import";
          serviceConfig = {
            Type = "oneshot";
            ExecStart = "${autobeets}/bin/autobeets";
            User = cfg.user;
          };
          wantedBy = ["default.target"];
        };
        
        systemd.timers."autobeets" = {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnBootSec = "1m";
            OnUnitActiveSec = cfg.interval;
            Unit = "autobeets.service";
          };
        };
      };
}
