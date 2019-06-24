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

WABT_WAT1="$TMP_DIR/tmp_conv.wat"
WABT_WAT2="$TMP_DIR/tmp_conv_2.wat"
WABT_WAT3="$TMP_DIR/tmp_conv_3.wat"

REF_WAT1="$TMP_DIR/tmp_conv_4.wat"
REF_WAT2="$TMP_DIR/tmp_conv_5.wat"
REF_WAT3="$TMP_DIR/tmp_conv_6.wat"


WABT_WASM1="$TMP_DIR/tmp_conv.wasm"
WABT_WASM2="$TMP_DIR/tmp_conv_2.wasm"

REF_WASM1="$TMP_DIR/tmp_conv_4.wasm"
REF_WASM2="$TMP_DIR/tmp_conv_5.wasm"

wasm2wat "$WASM_FILE" -o "$WABT_WAT1"
wat2wasm "$WAT_FILE" -o "$WABT_WASM1"
wasm2wat "$WABT_WASM1" -o "$WABT_WAT2"
wat2wasm "$WABT_WAT2" -o "$WABT_WASM2"
wasm2wat "$WABT_WASM2" -o "$WABT_WAT3"

wasm -d "$WABT_WASM1"  -o "$REF_WAT1"
wasm -d "$WABT_WASM2"  -o "$REF_WAT2"

wasm -d "$WABT_WAT1"  -o "$REF_WASM1"
wasm -d "$WABT_WAT2"  -o "$REF_WASM2"

wasm -d "$REF_WASM2"  -o "$REF_WAT3"


cmp -s "$WAT_FILE" "$WABT_WAT1"
WAT_CMP=$?

cmp -s "$WABT_WAT1" "$WABT_WAT2"
WAT2_CMP=$?

cmp -s "$WABT_WAT2" "$WABT_WAT3"
WAT3_CMP=$?

cmp -s "$WAT_FILE" "$REF_WAT1"
WAT4_CMP=$?

cmp -s "$WAT_FILE" "$REF_WAT2"
WAT5_CMP=$?

cmp -s "$WAT_FILE" "$REF_WAT3"
WAT6_CMP=$?


cmp -s "$WASM_FILE" "$WABT_WASM1"
WASM_CMP=$?

cmp -s "$WABT_WASM1" "$WABT_WASM2"
WASM2_CMP=$?

cmp -s "$WASM_FILE" "$REF_WASM1"
WASM4_CMP=$?

cmp -s "$WASM_FILE" "$REF_WASM2"
WASM5_CMP=$?

# if [ "$WAT_CMP" != 0 ]
# then
#   echo "wat"
#   exit 1
# elif [ "$WAT2_CMP" != 0 ]
# then
#   echo "wat2"
#   exit 1
if [ "$WAT3_CMP" != 0 ]
then
  echo "wat3"
  exit 1
elif [ "$WAT4_CMP" != 0 ]
then
  echo "wat4"
  exit 1
elif [ "$WAT5_CMP" != 0 ]
then
  echo "wat5"
  exit 1
elif [ "$WAT6_CMP" != 0 ]
then
  echo "wat6"
  exit 1
# elif [ "$WASM_CMP" != 0 ]
# then
#   echo "wasm"
#   exit 1
elif [ "$WASM2_CMP" != 0 ]
then
  echo "wasm2"
  exit 1
elif [ "$WASM4_CMP" != 0 ]
then
  echo "wasm4"
  exit 1
elif [ "$WASM5_CMP" != 0 ]
then
  echo "wasm5"
  exit 1
else
  exit 0
fi