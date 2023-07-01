let
    # inherit (import ./chr/pin.nix) pkgs;
    # bem
    #     =
    #     import (fetchTarball
    #         "https://github.com/monadosquito/bem/archive/f6446141daac31ff0ace748d8bdc4bd25d8bd109.tar.gz") {};
    pin = import ./chr/pin.nix;
    misoPkgs = pin.misoPkgs;
    bem = import ../bem {};
    misoBem = import ../miso-bem;
    comm = import ./chr/comm.nix pkgs;
    pkgs =
        import (fetchTarball
            https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz) {};
in
pkgs.mkShell
{
    buildInputs
        =
        [
            # (pkgs.haskell.packages.ghc8107.ghcWithPackages
            (misoPkgs.haskell.packages.ghc865.ghcWithPackages
                 (pkgs: with pkgs;
                      [
                          bem
                          ghcjs-dom
                          hspec
                          jsaddle-warp
                          miso
                          misoBem
                          quickcheck-instances
                          split
                          wai
                          websockets
                          free
                      ]
                 )
            )
            (pkgs.writeShellScriptBin
                 "gcroot-dev-deps-outs"
                 (pkgs.lib.readFile ./script/gcroot-dev-deps-outs.sh)
            )
            (pkgs.writeShellScriptBin
                 "unit-test"
                 (pkgs.lib.readFile ./script/test.sh)
            )
            (pkgs.writeShellScriptBin
                 "watch"
                 (pkgs.lib.readFile ./script/watch.sh)
            )
            pkgs.ghcid
        ]
        ++ comm.deps.execs;
    jsDepStorePaths = comm.deps.lclDrvs.jsLibDep.jsLibDeps;
    shellHook = pkgs.lib.readFile ./script/shell-hook.sh;
    usedJsDepScriptLclPaths
        =
        [
            "three/build/three.js"
            "three/examples/js/controls/OrbitControls.js"
            "three/examples/js/loaders/GLTFLoader.js"
        ];
} // comm.deps.lclDrvs
