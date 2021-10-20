# Plutus Fake PAB

In the CardStarter LiquidityBridge project we use a custom implemented PAB as the official one is not ready yet. This is in a private repo in the moment, sorry.

At the moment of writing this article, these are the features that the fake PAB supports:

- query utxos at an address, returning the same type as utxosAt from the plutus lib
- build tx based on UnBalancedTx and ScriptLookups from the plutus lib
- pre balance tx (adding minimum amount of tx inputs based on fee and tx output value, balancing non ada outputs)
- mint tokens, and send them to arbitrary address(es)
- redeem utxos from validator scripts, using the correct datum and redeemer (scripts, datums and redeemers are persisted in files for now)

And these are the things it doesn't support:

- wallet integration
- chain-index integration (handling datums outside our own ones)
- handling on chain events (utxo set change, waiting for slot, etc.)
- contract interpreter
- mutlisig

And a few things that could be better:

- using ogmios instead of calling the cardano-cli (and getting rid of ssh calls altogether - we use ssh to call nodes on servers we control)
- picking most suitable tx inputs to minimize fees
- calculating fees and minimum utxo lovelace amounts (it is hardcoded at the moment)
- its not tested thoroughly

## How to use this?

At the moment, we don't have a Contract interpreter, so for each endpoint, we need to create a copy of the off-chain code.

Here's a simple Contract:

```haskell
paymentConfirmationERCFromPubKey ::
  PubKeyHash ->
  PCEParams ->
  Contract () LiquidityBridgeSchema ContractError ()
paymentConfirmationERCFromPubKey serverPkh params = do
  utxos <- utxosAt $ Ledger.scriptAddress $ validator serverPkh
  lookups =
    Constraints.otherScript (validator server)
      <> Constraints.unspentOutputs utxos
      <> Constraints.otherData Ledger.unitDatum

  tx =
    TxConstraints.mustPayToPubKey recip val
      <> mconcat
        (map (`TxConstraints.mustSpendScriptOutput` Ledger.unitRedeemer) (Map.keys utxos))

  void $ submitTxConstraintsWith @Void lookups tx
```

In the fake PAB, we can use the same lookups and constraints. Also, the submitTx and utxosAt are using the same types:

```haskell
-- | The handler logic is almost the same as the contract.
handlePaymentConfirmationERC ::
  PABConfig ->
  Wallet ->
  ContractInstanceId ->
  State ->
  PubKeyHash ->
  PCEParams ->
  IO (Maybe Text) -- This function returns an IO monad instead of a Contract
handlePaymentConfirmationERC pabConf wallet contractInstanceID state params = do
  -- We need to use our implementation of utxosAt which takes a PABConfig
  -- as its first argument. Other arguments are exactly the same
  utxos <- utxosAt pabConf $ Ledger.scriptAddress $ validator serverPkh
  lookups =
    Constraints.otherScript (validator server)
      <> Constraints.unspentOutputs utxos
      <> Constraints.otherData Ledger.unitDatum

  tx =
    TxConstraints.mustPayToPubKey recip val
      <> mconcat
        (map (`TxConstraints.mustSpendScriptOutput` Ledger.unitRedeemer) (Map.keys utxos))

  submitTx @Void pabConf wallet lookups tx
```

Once we have our handlers, we need to wire them up:

```haskell
-- Endpoint schema, containing our endpoints and parameters
-- (these will be the request body parameters for contract activations)
data LiquidityBridgeContracts
  = PaymentConfirmationLP PCLParams
  | PaymentConfirmationERC PCEParams
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Fake PAB API Schema, stripped endpoints that we don't use in this project
type API =
  ("ws" :> WebSocketPending) -- Combined websocket (subscription protocol)
    :<|> ( "api"
            :> "contract"
            :> "activate"
            :> ReqBody '[JSON] (ContractActivationArgs LiquidityBridgeContracts) -- The contract schema defined above
            :> Post '[JSON] ContractInstanceId -- Start a new instance.


-- | This handler will call the corresponding contract endpoint handler
activateContractHandler ::
  PABConfig -> State -> ContractActivationArgs LiquidityBridgeContracts -> Handler ContractInstanceId
activateContractHandler pabConfig state (ContractActivationArgs cardMessage _) =
  case cardMessage of
    PaymentConfirmationLP params ->
      wrapHandler $ handlePaymentConfirmationLP pabConf wallet params
    PaymentConfirmationERC params ->
      wrapHandler $ handlePaymentConfirmationERC pabConf wallet params
```

Some of the boilerplate will disappear in the following days, and hopefully we will have a Contract interpreter, so we won't need to re-implement them in the fake PAB.

And finally, we supply the inital state, configurations, and the protocol params. If you use this with a private testnet, but want to run it on your local machine, you can set the pcCliLocation to Remote, as in the example below:

```haskell
  state <- State <$> newTVarIO Map.empty
  protocolParams <- JSON.decode <$> LazyByteString.readFile "protocol.json"
  let pabConfig =
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
          , -- File name where the transaction body will be saved
            pcTxBodyFile = "tx.body"
          , -- File name where the signed transaction will be saved
            pcTxFile = "tx.signed"
          }

  run 9080 (app pabConfig state)
```

To run the fake PAB, you need to prepare a few more things:

1. Query the protocol params with the cli

```bash
cardano-cli query protocol-parameters --testnet-magic 42 --out-file protocol.json
```

1. Copy this file to your local machine

```bash
scp 11.22.33.44:~/protocol.json ./
```

## How does this work and how it is structured?

The fake PAB consists of the following modules:

- **LiquidityBridgeFakePAB** main entry point, Contract re-implementation, handling websocket connections and http endpoints
- **LiquidityBridgeFakePAB.Constraints** handling constraints by creating the necessary files and calling cardano-cli commands
- **LiquidityBridgeFakePAB.CardanoCLI** wrappers for cardano-cli commands
- For development purposes, I created an ssh wrapper, so I can call relay these commands through an ssh connection. This is not nice, unsafe, and pretty slow, so I'm hoping to get rid of this pretty soon.
- **LiquidityBridgeFakePAB.UtxoParser** parse the output of the `cardano-cli query utxo` command
- **LiquidityBridgeFakePAB.Files** functions for handling script, datum and redeemer files
- **LiquidityBridgeFakePAB.Types** configuration for the fake pab
- **LiquidityBridgeFakePAB.PreBalance** prepare a transaction before sending to the cli for balancing. This includes:
  - adding tx inputs to cover fees and outputs
  - adding collaterals,
  - modifying tx outs to contain the minimum amount of lovelaces
  - balancing non ada outputs
