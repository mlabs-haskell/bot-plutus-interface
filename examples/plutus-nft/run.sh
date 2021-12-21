curl --location --request POST 'localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
      "caID": {
          "unTokenName": "TestToken"
      }
  }'
