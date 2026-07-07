{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    with flake-utils.lib;
    eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      in
      with pkgs;
      {
        packages.default = haskellPackages.nix-lang;
        packages.nix-lang = haskellPackages.nix-lang;
        packages.nix-lang-qq = haskellPackages.nix-lang-qq;
        devShells.default =
          with haskell.lib;
          (addBuildTools (haskellPackages.nix-lang) [
            haskell-language-server
            cabal-install
          ]).envFunc
            { };
      }
    )
    // {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
            hself: hsuper: {
              nix-lang =
                final.haskell.lib.overrideCabal
                  (final.haskell.lib.addTestToolDepends (prev.haskellPackages.callCabal2nix "nix-lang" ./. { }) [
                    final.nixfmt
                    # we'll run the parser on nixpkgs in test
                    final.nix
                  ])
                  (old: {
                    preCheck = (old.preCheck or "") + ''
                      export NIX_PATH="nixpkgs=${final.path}"
                    '';
                  });
              nix-lang-qq = hself.callCabal2nix "nix-lang-qq" ./nix-lang-qq { };
            }
          );
        });
      };
    };
}
