comm: misoPkgs: pkgs:
(pkgs.stdenv.mkDerivation
{
    buildPhase = pkgs.lib.readFile ../script/bld.sh;
    comm = ../src/comm;
    installPhase = pkgs.lib.readFile ../script/instl.sh;
    name = "project";
    nativeBuildInputs
        =
        [
            (misoPkgs.haskell.packages.ghcjs.ghcWithPackages
            # (pkgs.haskell.packages.ghcjs.ghcWithPackages
                (pkgs: with pkgs; [ghcjs-dom miso])
            )
            pkgs.closurecompiler
        ] ++ comm.deps.execs;
    outputs = ["out"];
    phases = ["buildPhase" "installPhase"];
    src = ../src/main;
    inherit (comm) data;
}
).overrideAttrs (_: comm.deps.lclDrvs)
