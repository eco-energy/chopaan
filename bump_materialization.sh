#!/bin/sh

nix build .#gcroot -o shell.gcroot --no-net

for f in shell.gcroot/materializers/*; do echo "$(basename $f) - $($f/calculateSha)"; $f/generateMaterialized nix/materialized/flake/$(basename $f); done


#nix-build -A passthru.calculateMaterializedSha | bas
#nix-build -A passthru.updateMaterialized | bash
