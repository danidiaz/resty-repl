# restyrepl

# Building

Use a modern version (>=2) of [cabal-install](https://www.haskell.org/cabal/).

To build:

    cabal new-build 

To run: 

    cabal new-exec -- resty-repl --help
    cabal new-exec -- resty-repl -p 8000 -e "/usr/local/bin/ghci"

# Motivation

[This seemed like a good
idea](https://www.reddit.com/r/haskell/comments/948s9q/running_ghci_in_background/).
