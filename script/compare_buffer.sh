#!/bin/bash

if [ "$#" -lt 1 ] || ! [ -f "$1" ]; then
  echo "Usage: $0 FILE" >&2
  exit 1
fi

TMP_DIR="tmp"

if ! [ -d "$TMP_DIR" ]
then
    mkdir "$TMP_DIR"
fi

WAT_FILE="$1"
WASM_FILE="$TMP_DIR/tmp_module.wasm"
JS_CH_FILE="$TMP_DIR/tmp_ch.js"
JS_JSC_FILE="$TMP_DIR/tmp_jsc.js"
JS_SM_FILE="$TMP_DIR/tmp_sm.js"
JS_V8_FILE="$TMP_DIR/tmp_v8.js"

TMP_CH="$TMP_DIR/tmp_ch"
TMP_JSC="$TMP_DIR/tmp_jsc"
TMP_SM="$TMP_DIR/tmp_sm"
TMP_V8="$TMP_DIR/tmp_v8"

TMP_CH_JSC="$TMP_DIR/tmp_ch_jsc"
TMP_JSC_SM="$TMP_DIR/tmp_jsc_sm"
TMP_SM_V8="$TMP_DIR/tmp_sm_v8"

ERROR_FILE="$TMP_DIR/error"

wat2wasm "$WAT_FILE" -o "$WASM_FILE" 2> "$ERROR_FILE"

ERROR=$?
if [ "$ERROR" != 0 ]
then
    exit "$ERROR"
fi

node ../javascript/convert.js "$WASM_FILE" "ch" > "$JS_CH_FILE" 2> "$ERROR_FILE"
node ../javascript/convert.js "$WASM_FILE" "jsc" > "$JS_JSC_FILE" 2> "$ERROR_FILE"
node ../javascript/convert.js "$WASM_FILE" "sm" > "$JS_SM_FILE" 2> "$ERROR_FILE"
node ../javascript/convert.js "$WASM_FILE" "v8" > "$JS_V8_FILE" 2> "$ERROR_FILE"

ERROR=$?
if [ "$ERROR" != 0 ]
then
    exit "$ERROR"
fi
timeout 1 ch "$JS_CH_FILE" > $TMP_CH 2>&1
timeout 1 jsc "$JS_JSC_FILE" > $TMP_JSC 2>&1
timeout 1 sm "$JS_SM_FILE" > $TMP_SM 2>&1
timeout 1 v8 "$JS_V8_FILE" > $TMP_V8 2>&1

cmp -n 5000 $TMP_CH $TMP_JSC > $TMP_CH_JSC 2>&1
cmp -n 5000 $TMP_JSC $TMP_SM > $TMP_JSC_SM 2>&1
cmp -n 5000 $TMP_SM $TMP_V8 > $TMP_SM_V8 2>&1

if [ -s "$TMP_CH_JSC" ] || [ -s "$TMP_JSC_SM" ] || [ -s "$TMP_SM_V8" ]
then
  exit 1
else
  exit 0
fi
