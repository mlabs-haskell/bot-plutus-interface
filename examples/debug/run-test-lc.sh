#!/usr/bin/env bash

set -euxo pipefail

# clear
cd ../..
pwd
cabal new-run exe:debug-run \
  "/home/mike/dev/mlabs/plutip-fixed-dir/bot-plutus-interface" \
  "/home/mike/dev/mlabs/net-setups/testnet-bpi-setup/binaries" \
  "/home/mike/dev/mlabs/plutip-fixed-dir/node/node.socket" \
  0 \
  $1