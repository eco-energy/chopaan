#!/bin/sh

nix-build -A passthru.calculateMaterializedSha | bash
nix-build -A passthru.updateMaterialized | bash
