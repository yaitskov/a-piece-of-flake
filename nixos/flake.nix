pof:
{ config
, lib
, pkgs
, ...
}: let
  cfg = config.services.a-piece-of-flake-fetcher;
   inherit (lib) mkOption types optionals;
  inherit (types) ints;
in {
  options.services.a-piece-of-flake-fetcher = {
    enable = lib.mkEnableOption "a-piece-of-flake-fetcher";
    ws-url = mkOption {
      type = types.str;
      default = "https://pieceofflakenixrepository.org:443";
      description = "base url to piece-of-flake web service";
    };
    secret-path = mkOption {
      type = types.path;
      description = "path to file with a fetche secret to be used on ws for auth";
    };
    nix-raw-cache = mkOption {
      type = types.path;
      default = "/var/piece-of-flake/nix-raw-cache";
      description = "nix raw cache";
    };
  };
  config = lib.mkIf cfg.enable {
    systemd.tmpfiles.rules = [ "d ${cfg.nix-raw-cache} 0770 a-piece-of-flake a-piece-of-flake -" ];
    users = {
      groups.a-piece-of-flake = {};
      users.a-piece-of-flake = {
        group = "a-piece-of-flake";
        isSystemUser = true;
      };
    };
    environment.systemPackages = [ pof ];
    systemd.services.a-piece-of-flake-fetcher = {
      wantedBy = [ "network-online.target" ];
      requires = [ "network-online.target" ];
      enable = true;
      path = [ pkgs.nix ];

      serviceConfig = {
        User = "a-piece-of-flake";
        Group = "a-piece-of-flake";
        Restart = "always";
        RestartSec = "8s";
        ExecStart =
          let
            ops = [ "-u" (toString cfg.ws-url)
                    "-c" (toString cfg.nix-raw-cache)
                    "-s" (toString cfg.secret-path)
                  ];
          in "${pof}/bin/a-piece-of-flake fetcher ${lib.escapeShellArgs ops}";
      };
    };
  };
}
