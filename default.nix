# Run like this:
#   nix-build /path/to/this/directory
# ... build products will be in ./result

{ pkgs ? (import <nixpkgs> {}), source ? ./., version ? "dev" }:

with pkgs;

stdenv.mkDerivation rec {
  name = "athens-${version}";
  src = lib.cleanSource source;

  buildInputs = [ makeWrapper ccl openssl libxml2 ];
  inherit version openssl libxml2;

  preBuild = ''
    make clean
    export LD_LIBRARY_PATH=${lib.makeLibraryPath [ openssl libxml2 ]}
    export XDG_CACHE_HOME="$TMP/.cache"
    export CL_SOURCE_REGISTRY="(:source-registry :ignore-inherited-configuration)"
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/athens $out/bin/athens.orig
    makeWrapper $out/bin/athens.orig $out/bin/athens \
        --suffix LD_LIBRARY_PATH : $LD_LIBRARY_PATH
  '';

  dontStrip = true;
}
