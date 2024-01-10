data:
let
    pin = import ./pin.nix;
    nixpkgs = import pin.nixpkgs {};
in
nixpkgs.stdenv.mkDerivation
{
    installPhase = nixpkgs.lib.readFile ../script/instl-js-lib-dep.sh;
    jsLibDeps =
        [
            (nixpkgs.nodePackages.three.overrideAttrs
                (oldAttrs:
                    {
                        installPhase = oldAttrs.installPhase
                            + nixpkgs.lib.readFile ../script/three-instl-hook.sh;
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
