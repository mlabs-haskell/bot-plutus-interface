# Various issue related notes and plans

[Issue](https://github.com/mlabs-haskell/bot-plutus-interface/issues/89)

[Initial plan](https://github.com/mlabs-haskell/bot-plutus-interface/issues/89#issuecomment-1142269067)

Places that seems like will need to filter collateral out:

* src
  * [ ] [Effects.hs](./src/BotPlutusInterface/Effects.hs#L171)
  * [ ] [ChainIndex.hs - 1](./src/BotPlutusInterface/ChainIndex.hs#L41)
  * [ ] [ChainIndex.hs - 2](./src/BotPlutusInterface/ChainIndex.hs#L45)
  * [ ] [ChainIndex.hs - 3](./src/BotPlutusInterface/ChainIndex.hs#L47)
  * [ ] [ChainIndex.hs - 4](./src/BotPlutusInterface/ChainIndex.hs#L52)
  * [ ] [ChainIndex.hs - 5](./src/BotPlutusInterface/ChainIndex.hs#L60)

* test
  * [ ] [MockContract.hs](./test/Spec/MockContract.hs#L567)

## Considerations

* tests - mocking need to be enhanced to support tests for collateral filtering
* originally during balancing Tx for creating collateral UTxO collateral and change output were merged in single one, to prevent this additional check in filtering was added in [addAdaChange](./src/BotPlutusInterface/Balance.hs#L392) and [hasChangeUTxO](./src/BotPlutusInterface/Balance.hs#L376)
