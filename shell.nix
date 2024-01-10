let
    pin = import ./chr/pin.nix;
    nixpkgs = import pin.nixpkgs {inherit config;};
    bem = import ../bem {};
    misoBem = import ../miso-bem;
    comm = import ./chr/comm.nix nixpkgs;
    config = {
        allowBroken = true;
    };
in
nixpkgs.mkShell
{
    buildInputs
        =
        [
            (nixpkgs.haskellPackages.ghcWithPackages
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
            (nixpkgs.writeShellScriptBin
                 "gcroot-dev-deps-outs"
                 (nixpkgs.lib.readFile ./script/gcroot-dev-deps-outs.sh)
            )
            (nixpkgs.writeShellScriptBin
                 "unit-test"
                 (nixpkgs.lib.readFile ./script/test.sh)
            )
            (nixpkgs.writeShellScriptBin
                 "watch"
                 (nixpkgs.lib.readFile ./script/watch.sh)
            )
            nixpkgs.ghcid
        ]
        ++ comm.deps.execs;
    jsDepStorePaths = comm.deps.lclDrvs.jsLibDep.jsLibDeps;
    shellHook = nixpkgs.lib.readFile ./script/shell-hook.sh;
    usedJsDepScriptLclPaths
        =
        [
            "three/build/three.js"
            "three/examples/js/controls/OrbitControls.js"
            "three/examples/js/loaders/GLTFLoader.js"
        ];
} // comm.deps.lclDrvs
