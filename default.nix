{pkgs ? import ./nixpkgs.nix }
:
let
  inherit(pkgs) stdenv lib;
  inherit(pkgs) sbt;
in
with stdenv.lib;
stdenv.mkDerivation rec {
  name = "fpinscala";

  version = "1.0";

  buildInputs = [ sbt ];

  SBT_OPTS = [
    "-Xmx1536M"
    "-XX:+UseConcMarkSweepGC"
    "-XX:+CMSClassUnloadingEnabled"
    "-Xss2M"
  ];

  shellHook = ''
    echo
    echo "---- ${name}-${version}  environment up and running ----"
    echo "run      Runs 'sbt run'"
    echo "runTests Runs 'sbt test'"

    alias run='sbt run'
    alias runTests='sbt test'
  '';

  meta = {
    description = "Solutions for eBook exercises 'Functional programming in scala'";
    longDescription = ''
      Solutions for eBook exercises 'Functional programming in scala'
    '';
    license = with licenses; unfree;
    maintainers = with maintainers; periklis;
    platforms = with platforms; linux ++ darwin;
  };
}
