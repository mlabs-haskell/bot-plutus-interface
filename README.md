# MLabPAB (Plutus Fake PAB)

This is a custom implementation of the PAB as the official one is not ready yet.
This PAB is not feature complete, and not fully tested with all use cases, so please use it with care!

Disclaimer: this project is still a work in progress, with a few missing features. We don't take any responsibility for any losses, only use it at your own risk!

Supported features:

- query utxos at an address (utxosAt)
- evaluate Contract (not all features implemented, see the rest of this list for details)
- pre balance tx (adding minimum amount of tx inputs based on fee and tx output value, balancing non ada outputs)
- mint tokens, and send them to arbitrary address(es)
- redeem utxos from validator scripts, using the correct datum and redeemer (scripts, datums and redeemers are persisted in files for now)

Unsupported/In development

- wallet integration
- chain-index integration (in progress)
- handling on-chain events (utxo set change, waiting for slot, etc.)
- multisig
- automated tests

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

3. Write your main entrypoint for the application, with the preferred configurations

```haskell
main :: IO ()
main = do
  protocolParams <- JSON.decode <$> LazyByteString.readFile "protocol.json"
  let pabConf =
        PABConfig
          { -- Calling the cli through ssh when set to Remote
            pcCliLocation = Remote "11.22.33.44"
          , pcNetwork = Testnet (NetworkMagic 42)
          , pcProtocolParams = protocolParams
          , -- Directory name of the script and data files
            pcScriptFileDir = "result-scripts"
          , -- Dry run mode will build the tx, but skip the submit step
            pcDryRun = False
          , -- Protocol params file location relative to the cardano-cli working directory (needed for the cli)
            pcProtocolParamsFile = "./protocol.json"
          }
  MLabsPAB.runPAB @MyContracts pabConf
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

- **MLabsPAB** main entry point
- **MLabsPAB.Server** Servant server, handling http endpoint calls and websockets
- **MLabsPAB.Contract** handling contract effects by creating the necessary files and calling cardano-cli commands (a few effects are mocked)
- **MLabsPAB.PreBalance** doing some preparations so the cli can process the rest (non-ada asset balancing, addig tx inputs, adding minimum lovelaces, add signatories)
- **MLabsPAB.CardanoCLI** wrappers for cardano-cli commands
- For development purposes, I created an ssh wrapper, so I can call relay these commands through an ssh connection. This is not nice, unsafe, and pretty slow, so I'm hoping to get rid of this pretty soon.
- **MLabsPAB.UtxoParser** parse the output of the `cardano-cli query utxo` command
- **MLabsPAB.Files** functions for handling script, datum and redeemer files
- **MLabsPAB.Types** configuration for the fake pab
- **MLabsPAB.PreBalance** prepare a transaction before sending to the cli for balancing. This includes:
  - adding tx inputs to cover fees and outputs
  - adding collaterals,
  - modifying tx outs to contain the minimum amount of lovelaces
  - balancing non ada outputs
