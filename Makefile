# The plutus-pab commands, contracts and hoogle environment
# are made availible by the nix shell defined in shell.nix.
# In most cases you should execute Make after entering nix-shell.

.PHONY: hoogle pab_servers_all pab_servers_all pab_db clean_db \
	nix_build_lib nix_build_examples build test accept_pirs watch ghci readme_contents \
	format lint requires_nix_shell

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available options:"
	@echo "  WALLET  -- Specify wallet for command (1 or 2)"
	@echo "  FLAGS   -- Additional options passed to --ghc-options"
	@echo "  NIXOS   -- Add stack flags --no-nix and --system-ghc to work around stack issues on NixOS"
	@echo
	@echo "Available commands:"
	@echo "  hoogle              -- Start local hoogle"
	@echo "  nix_build_lib       -- Run nix build -L on the library and tests"
	@echo "  nix_build_examples  -- Run nix build -L on all examples"
	@echo "  build               -- Run cabal v2-build"
	@echo "  watch               -- Track files: bot-plutus-interface.cabal, src/* and run 'make build' on change"
	@echo "  test                -- Run cabal v2-test"
	@echo "  accept_pirs         -- Accept new PIR changes"
	@echo "  ghci                -- Run cabal v2-repl bot-plutus-interface"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  cabalfmt            -- Apply cabal formatting with cabal-fmt"
	@echo "  cabalfmt_check      -- Check cabal files for formatting errors without making changes"
	@echo "  nixpkgsfmt          -- Apply nix formatting with nixfmt"
	@echo "  nixpkgsfmt_check    -- Check nix files for format errors"
	@echo "  lint                -- Check the sources with hlint"
	@echo "  readme_contents     -- Add table of contents to README"
	@echo "  update_plutus       -- Update plutus version with niv"
	@echo "  clear_build         -- Deletes the build files for this specific project"

# Need to use --no-nix and --system-ghc from inside nix-shell
# on NixOS since stack doesn't support nixos, and ghc8103 isnt in any nixpkgs version
# maybe we could support this via stack's nix integration using a separate stack_shell.nix
ifdef NIXOS
STACK_FLAGS = --no-nix --system-ghc
endif

hoogle: requires_nix_shell
	hoogle server --local --port=8070 > /dev/null &

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

nix_build_lib:
	nix build -L .#check.x86_64-linux

nix_build_examples:
	nix build -L .#plutus-transfer:exe:plutus-transfer-pab .#plutus-game:exe:plutus-game-pab .#plutus-nft:exe:plutus-nft-pab

build: requires_nix_shell
	cabal v2-build $(GHC_FLAGS)

watch: requires_nix_shell
	while sleep 1; do find src bot-plutus-interface.cabal | entr -cd make build; done

test: requires_nix_shell
	cabal v2-test

accept_pirs: requires_nix_shell
	stack build --test $(STACK_FLAGS) $(GHC_FLAGS) --ta '-p MarketAction --accept'

ghci: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) bot-plutus-interface

# Source dirs to run fourmolu on
FORMAT_SOURCES := $(shell fd -e hs)

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor

# Run fourmolu formatter
format: requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Check formatting (without making changes)
format_check: requires_nix_shell
	fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Cabal package definitions
CABAL_SOURCES := $(shell fd -e cabal)

cabalfmt: requires_nix_shell
	cabal-fmt --inplace $(CABAL_SOURCES)

cabalfmt_check: requires_nix_shell
	cabal-fmt --check $(CABAL_SOURCES)

# Nix files to format
NIX_SOURCES := $(shell fd -e nix)

nixpkgsfmt: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

nixpkgsfmt_check: requires_nix_shell
	nixpkgs-fmt --check $(NIX_SOURCES)

# Check with hlint, currently I couldn't get --refactor to work
lint: requires_nix_shell
	hlint $(FORMAT_SOURCES)

readme_contents:
	echo "this command is not nix-ified, you may receive an error from npx"
	npx markdown-toc ./README.md --no-firsth1

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix-shell --pure' first" && false)


PLUTUS_BRANCH = $(shell jq '.plutus.branch' ./nix/sources.json )
PLUTUS_REPO = $(shell jq '.plutus.owner + "/" + .plutus.repo' ./nix/sources.json )
PLUTUS_REV = $(shell jq '.plutus.rev' ./nix/sources.json )
PLUTUS_SHA256 = $(shell jq '.plutus.sha256' ./nix/sources.json )

update_plutus:
	@echo "Updating plutus version to latest commit at $(PLUTUS_REPO) $(PLUTUS_BRANCH)"
	niv update plutus
	@echo "Latest commit: $(PLUTUS_REV)"
	@echo "Sha256: $(PLUTUS_SHA256)"
	@echo "Make sure to update the plutus rev in stack.yaml with:"
	@echo "    commit: $(PLUTUS_REV)"
	@echo "This may require further resolution of dependency versions."

################################################################################
# Geth

build_path = dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/bot-plutus-interface-0.1
clear_build:
	@[ ! -e $(build_path) ] || rm -rf $(build_path)
