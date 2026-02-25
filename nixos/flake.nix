lfi:
{ config
, lib
, pkgs
, ...
}: let
  cfg = config.programs.a-piece-of-flake;
   inherit (lib) mkOption types optionals;
  inherit (types) ints;
in {
  options.programs.a-piece-of-flake = {
    enable = lib.mkEnableOption "a-piece-of-flake";
    port = mkOption {
      type = types.port;
      default = 8800;
      description = "HTTP port - service port";
    };
    cert = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "path to file with a certificate chain for HTTPS; HTTP is used if not set";
    };
    cert-key = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "path to file with a certificate private key for HTTPS; HTTP is used if not set";
    };
  };
  config = lib.mkIf cfg.enable {
    users = {
      groups.a-piece-of-flake = {};
      users.a-piece-of-flake = {
        group = "a-piece-of-flake";
        isSystemUser = true;
      };
    };
    environment.systemPackages = [ lfi ];
    systemd.services.a-piece-of-flake = {
      wantedBy = [ "network-online.target" ];
      requires = [ "network-online.target" ];
      enable = true;
      serviceConfig = {
        User = "a-piece-of-flake";
        Group = "a-piece-of-flake";
        Restart = "always";
        RestartSec = "8s";
        ExecStart =
          let
            ops = ["-p" (toString cfg.port)
                  ]
                  ++ optionals (cfg.cert != null) [ "-g" cfg.cert ]
                  ++ optionals (cfg.cert-key != null) [ "-d" cfg.cert-key ];
          in "${lfi}/bin/a-piece-of-flake run ${lib.escapeShellArgs ops}";
      };
    };
  };
}
