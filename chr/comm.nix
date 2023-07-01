pkgs:
rec
{
    data = ../data;
    deps
        =
        {
            execs = with pkgs.nodePackages; [sass]; 
            lclDrvs
                =
                {
                    jsLibDep = import ./js-lib-dep.nix data;
                    misoBem = import ../../miso-bem/default.nix;
                };
        };
}
