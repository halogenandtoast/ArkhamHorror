with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    zlib
    haskellPackages.postgresql-libpq
    postgresql
  ];
}
