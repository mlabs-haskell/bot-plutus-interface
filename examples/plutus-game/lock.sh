#!/bin/sh

CONTRACT_INST_ID=$(curl --location --request POST 'localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
    "caID": {
        "tag": "Lock",
        "contents": {
            "lockGameId": 3,
            "lockAmount": 1000000,
            "lockSecret": "secret"
        }
    }
  }' | jq -r .unContractInstanceId )

echo $CONTRACT_INST_ID


echo "{ \"tag\": \"Subscribe\", \"contents\": { \"Left\": { \"unContractInstanceId\":\"$CONTRACT_INST_ID\" } } }" | websocat -n ws://localhost:9080/ws

