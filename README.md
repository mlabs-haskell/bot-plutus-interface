# Bot Plutus Interface (formerly MLabsPAB or Plutus Fake PAB)

This is a custom implementation of the PAB as the official one is not ready yet.
This PAB is not feature complete, and not fully tested with all use cases, so please use it with care!

Disclaimer: this project is still a work in progress, with a few missing features. We don't take any responsibility for any losses, only use it at your own risk!

Supported features:

- query utxos at an address (utxosAt)
- evaluate Contract (not all features implemented, see the rest of this list for details)
- pre balance tx (adding minimum amount of tx inputs based on fee and tx output value, balancing non ada outputs)
- mint tokens, and send them to arbitrary address(es)
- redeem utxos from validator scripts, using the correct datum and redeemer (scripts, datums and redeemers are persisted in files for now)
- use validity time ranges
- waiting for slots

Unsupported/In development

- wallet integration
- handling on-chain events (utxo set change, etc.)

## How to use this?

Wiring up your own contract is pretty much the same as with the official PAB:

1. Define your endpoints for the PAB

```haskell
data MyContracts
  = Send SendParams
  | Collect CollectParams
  deriving stock
    (Show, Generic)
  deriving anyclass
    (FromJSON, ToJSON)
```

2. Define a HasDefinitions instance for the endpoints

```haskell
import BotPlutusInterface.Types (HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import Playground.Types (FunctionSchema)
import Schema (FormSchema)

instance HasDefinitions MyContracts where
  getDefinitions :: [MyContract]
  getDefinitions = []

  getSchema :: MyContracts -> [FunctionSchema FormSchema]
  getSchema = \case
    Send _ -> endpointsToSchemas @MyContractSchema
    Collect _ -> endpointsToSchemas @MyContractSchema

  getContract :: (MyContracts -> SomeBuiltin)
  getContract = \case
    Send params ->
      SomeBuiltin $
        MyContract.contract params
    Collect params ->
      SomeBuiltin $
        MyContract.contract params
```

3. Write your main entrypoint for the application and the configuration file

```haskell
import BotPlutusInterface qualified
import BotPlutusInterface.Config qualified as BotPlutusInterface
import Prelude

main :: IO ()
main = do
  pabConf <-
    either error id
      <$> BotPlutusInterface.loadPABConfig "./pabConfig.value"
  BotPlutusInterface.runPAB @MyContracts pabConf
```

Configuration format (example: <examples/plutus-game/pabConfig.value>): 

``` console
$ cabal repl --disable-optimisation --repl-options -Wwarn
...
BotPlutusInterface> :m Prelude
...
Prelude> :l BotPlutusInterface.Config
...
Prelude BotPlutusInterface.Config> putStrLn docPABConfig
Top-level configuration file fields:
    cliLocation: `local` or destination text
        calling the cli through ssh when set to destination (default:
        local)
    chainIndexUrl: url text
         (default: "http://localhost:9083")
    networkId: case insensitive `mainnet` atom or 32-bit unsigned integral number
         (default: 42)
    scriptFileDir: path text
        Directory name of the script and data files (default:
        "./result-scripts")
    signingKeyFileDir: path text
        Directory name of the signing key files (default: "./signing-keys")
    txFileDir: path text
        Directory name of the transaction files (default: "./txs")
    metadataDir: path text
        Directory name of metadata files (default: "/metadata")
    protocolParamsFile: filepath text
        Protocol params file location relative to the cardano-cli working
        directory (needed for the cli) in JSON format.  (default:
        "./protocol.json")
    dryRun: `true` or `false`
        Dry run mode will build the tx, but skip the submit step (default:
        true)
    logLevel: `error` or `warn` or `notice` or `info` or `debug`
         (default: info)
    ownPubKeyHash: PubKeyHash text
         (default: "")
    ownStakePubKeyHash: case insensitive `nothing` atom or StakePubKeyHash text
         (default: nothing)
    tipPollingInterval: non-negative integral number
         (default: 10000000)
    port: port non-negative integral number
         (default: 9080)
    enableTxEndpoint: `true` or `false`
         (default: false)
    collectStats: `true` or `false`
        Save some stats during contract run (only transactions execution
        budgets supported atm) (default: false)
    collectLogs: `true` or `false`
        Save logs from contract execution: pab request logs and contract
        logs (default: false)
```

To run the fake PAB, you need to prepare a few more things:

4. Save the protocol params file to the root folder of your project using the cardano-cli

```bash
cardano-cli query protocol-parameters --testnet-magic 42 --out-file protocol.json
```

5. Create a `signing-keys` folder under your projects root with the necessary signig key file(s).
   The files should be named in the following format: `signing-key-PUBKEYHASH.skey`

Use the cardano-cli to find out the pub key hash for your key:

```bash
cardano-cli address key-hash --verification-key-file VERIFICATION_KEY.vkey
```

## How does this work and how it is structured?

The fake PAB consists of the following modules:

- **BotPlutusInterface** main entry point
- **BotPlutusInterface.Server** Servant server, handling http endpoint calls and websockets
- **BotPlutusInterface.Config** load/save PAB configuration file
- **BotPlutusInterface.Contract** handling contract effects by creating the necessary files and calling cardano-cli commands (a few effects are mocked)
- **BotPlutusInterface.Balance** doing some preparations so the cli can process the rest (non-ada asset balancing, addig tx inputs, adding minimum lovelaces, add signatories)
- **BotPlutusInterface.CardanoCLI** wrappers for cardano-cli commands
- For development purposes, I created an ssh wrapper, so I can call relay these commands through an ssh connection. This is not nice, unsafe, and pretty slow, avoid using it if you can.
- **BotPlutusInterface.UtxoParser** parse the output of the `cardano-cli query utxo` command
- **BotPlutusInterface.Files** functions for handling script, datum and redeemer files
- **BotPlutusInterface.Types** configuration for the fake pab
- **BotPlutusInterface.Balance** prepare a transaction before sending to the cli for balancing. This includes:
  - adding tx inputs to cover fees and outputs
  - adding collaterals,
  - modifying tx outs to contain the minimum amount of lovelaces
  - balancing non ada outputs
