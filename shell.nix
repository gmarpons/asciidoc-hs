let pkgs = import (fetchTarball path) {};
    path = https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
in with pkgs;
  mkShell {
    buildInputs = [
      asciidoctor-with-extensions
      git
      haskell.compiler.ghc901
      haskellPackages.cabal-install
      haskellPackages.pandoc
      html-tidy
      icdiff
    ];
    shellHook = ''
      git config core.hooksPath .hooks

      function calldoctor() {
        if [[ -z "$2" ]] ; then
          echo "calldoctor [html4|html5] FILE"
          echo "Compares the output of asciidoctor/asciidoc-hs on AsciiDoc file FILE."
        else
          local FILE="$2"
          cabal build exe:asciidoc-hs
          icdiff <(
              asciidoctor -s -a "sectids!" -a "showtitle" $FILE -o - 2> /dev/null | tidy 2> /dev/null
            ) <(
              cat $FILE | cabal exec asciidoc-hs | pandoc -f json -t $1 | tidy 2> /dev/null
            )
        fi
      }
    '';
    LC_ALL="C.UTF-8";
  }
