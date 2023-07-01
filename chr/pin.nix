{
    # tmp

    misoPkgs = (import
               (builtins.fetchTarball
                    https://github.com/dmjio/miso/archive/1.7.1.tar.gz
               )
               {}
               ).pkgs;






    #######
    # WAS

    ###
    # misoPkgs =
    #     (import
    #         (builtins.fetchGit
    #             {
    #                 name = "miso-with-nixpkgs";
    #                 ref = "master";
    #                 rev = "339505587fd8576c7f89484d3a4244732a9f00e6";
    #                 url = https://github.com/dmjio/miso;
    #             }
    #         )
    #         {}
    #     ).pkgs;
    # pkgs
    #     =
    #     import (fetchTarball
    #         https://github.com/NixOS/nixpkgs/archive/7e9b0dff974c89e070da1ad85713ff3c20b0ca97.tar.gz) {};
    # checked -
    # pkgs = import
    #     (builtins.fetchGit
    #         {
    #             name = "nixpkgs-nixos-unstable";
    #             ref = "master";
    #             rev = "5aaed40d22f0d9376330b6fa413223435ad6fee5";
    #             url = https://github.com/NixOS/nixpkgs;
    #         }
    #     )
    #     {};
    # pkgs = import <nixpkgs> {};
    # checked -
    # pkgs
    #     =
    #     import (fetchTarball
    #         https://github.com/NixOS/nixpkgs/archive/5aaed40d22f0d9376330b6fa413223435ad6fee5.tar.gz) {};
    pkgs
        =
        import (fetchTarball
            https://github.com/NixOS/nixpkgs/archive/a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31.tar.gz) {};
}
