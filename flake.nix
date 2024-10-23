{
  description = "A very basic flake";

  inputs = {
    # Switch to stable... please.. less rebuilds
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
    let 
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };
      hs = pkgs.haskellPackages;
      pkg = hs.developPackage {
        root = ./.;
        overrides = self: super: {
          rdf4h = super.rdf4h.overrideAttrs (oldAttrs: nixpkgs.lib.recursiveUpdate oldAttrs {
            meta.broken = false;
          });
        };
      };
    in 
    {
      # debugging stuff
      packages.x86_64-linux.dotview = pkg;
      packages.x86_64-linux.testview = pkgs.haskellPackages.rdf4h;
      packages.x86_64-linux.default = self.packages.x86_64-linux.dotview;


      devShells.x86_64-linux.dotview = pkgs.haskellPackages.shellFor {
          packages = p: [
            pkg 
            # same as ghcEnv ?
          ];

          buildInputs = with pkgs.haskellPackages; [
            cabal-install

            # Helpful tools for `nix develop` shells
            #
            #ghcid                   # https://github.com/ndmitchell/ghcid
            haskell-language-server  # https://github.com/haskell/haskell-language-server
            #hlint                   # https://github.com/ndmitchell/hlint
            #ormolu                  # https://github.com/tweag/ormolu
          ];

          withHoogle = false;
        };
      devShells.x86_64-linux.default = self.devShells.x86_64-linux.dotview;

    };
}
