#!/usr/bin/env bash

set -euxo pipefail

# clear
cd ../..
pwd
cabal new-run exe:debug-run \
  "/home/mike/dev/dev-tmp/plutip-cluster/bot-plutus-interface" \
  "/home/mike/dev/mlabs/net-setups/binaries_1_35_3" \
  "/home/mike/dev/dev-tmp/plutip-cluster/pool-1/node.socket" \
  0 \
  $1