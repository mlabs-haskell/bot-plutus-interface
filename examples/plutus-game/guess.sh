#!/bin/sh

CONTRACT_INST_ID=$(curl --location --request POST 'localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
    "caID": {
        "tag": "Guess",
        "contents": {
            "guessGameId": 2,
            "guessSecret": "secret"
        }
    }
  }' | jq -r .unContractInstanceId )

echo $CONTRACT_INST_ID


echo "{ \"tag\": \"Subscribe\", \"contents\": { \"Left\": { \"unContractInstanceId\":\"$CONTRACT_INST_ID\" } } }" | websocat -n ws://localhost:9080/ws 

