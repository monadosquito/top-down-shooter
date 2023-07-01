let
    comm = import ./chr/comm.nix pin.pkgs;
    pin = import ./chr/pin.nix;
    project = import ./chr/project.nix comm pin.misoPkgs pin.pkgs;
in
{
    cold
        =
        project.overrideAttrs
        (oldAttrs:
            {
                outputs = ["cold"] ++ oldAttrs.outputs;
                platf = "cold";
            }
        );
    warm
        =
        project.overrideAttrs
        (oldAttrs:
            {
                outputs = ["warm"] ++ oldAttrs.outputs;
                platf = "warm";
            }
        );
}
