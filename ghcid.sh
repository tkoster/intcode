#!/bin/bash

# Install trap to restore cursor after Ctrl-C.
cnorm() {
  tput cnorm
}
trap cnorm EXIT INT

# Run ghcid.
stack exec ghcid -- \
  --warnings \
  --command "stack ghci intcode:lib intcode:test:intcode-test --main-is intcode:test:intcode-test --ghci-options -Wall $1" \
  --test="main"
