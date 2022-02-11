#!/bin/sh
CONTRACT_INST_ID=$(curl --location --request POST 'localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
      "caID": {
        "tfpOutputPerTx": 50,
        "tfpPayments": [
          [ {"getPubKeyHash": "981fc565bcf0c95c0cfa6ee6693875b60d529d87ed7082e9bf03c6a4"},
            {"getValue": [[{"unCurrencySymbol":""},[[{"unTokenName": ""}, 1500000]]]]}
          ]
        ]
      }
  }' | jq -r .unContractInstanceId )

echo $CONTRACT_INST_ID


echo "{ \"tag\": \"Subscribe\", \"contents\": { \"Left\": { \"unContractInstanceId\":\"$CONTRACT_INST_ID\" } } }" | websocat -n ws://localhost:9080/ws 

