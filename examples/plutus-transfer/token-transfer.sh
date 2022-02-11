#!/bin/sh
CONTRACT_INST_ID=$(curl --location --request POST 'localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
      "caID": {
        "tfpOutputPerTx": 2,
        "tfpPayments": [
          [ {"getPubKeyHash": "981fc565bcf0c95c0cfa6ee6693875b60d529d87ed7082e9bf03c6a4"},
            {"getValue": [[{"unCurrencySymbol":"1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"},[[{"unTokenName": "testToken"}, 15]]]]}
          ],
          [ {"getPubKeyHash": "6696936bb8ae24859d0c2e4d05584106601f58a5e9466282c8561b88"},
            {"getValue": [[{"unCurrencySymbol":"1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"},[[{"unTokenName": "testToken"}, 18]]]]}
          ],
          [ {"getPubKeyHash": "a11767a73ea3f59fb11f17c1627706115de75e2d2c444b0e43789567"},
            {"getValue": [[{"unCurrencySymbol":"1d6445ddeda578117f393848e685128f1e78ad0c4e48129c5964dc2e"},[[{"unTokenName": "testToken"}, 20]]]]}
          ]
        ]
      }
  }' | jq -r .unContractInstanceId )

echo $CONTRACT_INST_ID


echo "{ \"tag\": \"Subscribe\", \"contents\": { \"Left\": { \"unContractInstanceId\":\"$CONTRACT_INST_ID\" } } }" | websocat -n ws://localhost:9080/ws 

