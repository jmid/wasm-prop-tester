#!/bin/bash

if [ "$#" -lt 2 ] || ! [ -f "$1" ] || ! [ -f "$2" ]; then
  echo "Usage: $0 WAT_FILE WASM_FILE" >&2
  exit 1
fi

TMP_DIR="tmp"

if ! [ -d "$TMP_DIR" ]
then
    mkdir "$TMP_DIR"
fi

WAT_FILE="$1"
WASM_FILE="$2"

WAT_FROM_WASM_FILE="$TMP_DIR/tmp_conv.wat"
WAT_FROM_SECOND_WASM_FILE="$TMP_DIR/tmp_conv_2.wat"
WASM_FROM_WAT_FILE="$TMP_DIR/tmp_conv.wasm"
WASM_FROM_SECOND_WAT_FILE="$TMP_DIR/tmp_conv_2.wasm"

wasm -d "$WASM_FILE" -o "$WAT_FROM_WASM_FILE"
wasm -d "$WAT_FILE"  -o "$WASM_FROM_WAT_FILE"
wasm -d "$WASM_FROM_WAT_FILE" -o "$WAT_FROM_SECOND_WASM_FILE"
wasm -d "$WAT_FROM_SECOND_WASM_FILE"  -o "$WASM_FROM_SECOND_WAT_FILE"

cmp -s "$WAT_FILE" "$WAT_FROM_WASM_FILE"
WAT_CMP=$?

cmp -s "$WAT_FILE" "$WAT_FROM_SECOND_WASM_FILE"
WAT2_CMP=$?

cmp -s "$WASM_FILE" "$WASM_FROM_WAT_FILE"
WASM_CMP=$?

cmp -s "$WASM_FROM_WAT_FILE" "$WASM_FROM_SECOND_WAT_FILE"
WASM2_CMP=$?

if [ "$WAT_CMP" != 0 ]
then
  echo "wat"
  exit 1
elif [ "$WAT2_CMP" != 0 ]
then
  echo "wat2"
  exit 1
# elif [ "$WASM_CMP" != 0 ]
# then
#   echo "wasm"
#   exit 1
elif [ "$WASM2_CMP" != 0 ]
then
  echo "wasm2"
  exit 1
else
  exit 0
fi