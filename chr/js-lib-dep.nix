data:
let
    inherit (import ./pin.nix) pkgs;
in
pkgs.stdenv.mkDerivation
{
    installPhase = pkgs.lib.readFile ../script/instl-js-lib-dep.sh;
    jsLibDeps =
        [
            (pkgs.nodePackages.three.overrideAttrs
                (oldAttrs:
                    {
                        installPhase = oldAttrs.installPhase
                            + pkgs.lib.readFile ../script/three-instl-hook.sh;
                        name = "three";
                    }
                )
            )
        ];
    name = "js-lib-dep";
    phases = ["installPhase"];
    src = ../data/js-lib-used;
    inherit data;
}
