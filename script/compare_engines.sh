#!/bin/bash

if [ "$#" -lt 1 ] || ! [ -f "$1" ]; then
  echo "Usage: $0 wasm-file" >&2
  exit 1
fi

TMP_DIR="tmp"

if ! [ -d "$TMP_DIR" ]
then
    mkdir "$TMP_DIR"
fi

WASM_FILE="$1"
JS_CH_FILE="$TMP_DIR/tmp_ch.js"
JS_JSC_FILE="$TMP_DIR/tmp_jsc.js"
JS_SM_FILE="$TMP_DIR/tmp_sm.js"
JS_V8_FILE="$TMP_DIR/tmp_v8.js"

TMP_REF="$TMP_DIR/tmp_ref"
TMP_CH="$TMP_DIR/tmp_ch"
TMP_JSC="$TMP_DIR/tmp_jsc"
TMP_SM="$TMP_DIR/tmp_sm"
TMP_V8="$TMP_DIR/tmp_v8"

TMP_CH_JSC="$TMP_DIR/tmp_cmp_ch_jsc"
TMP_JSC_SM="$TMP_DIR/tmp_cmp_jsc_sm"
TMP_SM_V8="$TMP_DIR/tmp_cmp_sm_v8"

ERROR_FILE="$TMP_DIR/error"

node ../javascript/convert.js "$WASM_FILE" "ch"  > "$JS_CH_FILE"  2> "$ERROR_FILE"
node ../javascript/convert.js "$WASM_FILE" "jsc" > "$JS_JSC_FILE" 2> "$ERROR_FILE"
node ../javascript/convert.js "$WASM_FILE" "sm"  > "$JS_SM_FILE"  2> "$ERROR_FILE"
node ../javascript/convert.js "$WASM_FILE" "v8"  > "$JS_V8_FILE"  2> "$ERROR_FILE"

ERROR=$?
if [ "$ERROR" != 0 ]
then
    echo "conversion failed"
    exit "$ERROR"
fi

timeout 10 ch "$JS_CH_FILE" > "$TMP_CH" 2>&1
timeout 10 jsc "$JS_JSC_FILE" > "$TMP_JSC" 2>&1
timeout 10 sm "$JS_SM_FILE" > "$TMP_SM" 2>&1
timeout 10 v8 "$JS_V8_FILE" > "$TMP_V8" 2>&1

cmp -n 5000 $TMP_CH $TMP_JSC > $TMP_CH_JSC 2>&1
cmp -n 5000 $TMP_JSC $TMP_SM > $TMP_JSC_SM 2>&1
cmp -n 5000 $TMP_SM $TMP_V8 > $TMP_SM_V8 2>&1

if [ -s "$TMP_CH_JSC" ] || [ -s "$TMP_JSC_SM" ] || [ -s "$TMP_SM_V8" ];
then
  # Test buggy behaviour
    if [ $(grep -c 'tee_local' $TMP_CH) -gt 0 ];
    then
	exit 0
    else
	# Save diff result, to avoid last shrinking step overwriting it
	cp -f "$WASM_FILE" "$TMP_DIR/prev.wasm"
	cp -f "$JS_CH_FILE"  "$TMP_DIR/prev_ch.js"
	cp -f "$JS_JSC_FILE" "$TMP_DIR/prev_jsc.js"
	cp -f "$JS_SM_FILE"  "$TMP_DIR/prev_sm.js"
	cp -f "$JS_V8_FILE"  "$TMP_DIR/prev_v8.js"
	cp -f "$TMP_CH"  "$TMP_DIR/prev_ch"
	cp -f "$TMP_JSC" "$TMP_DIR/prev_jsc"
	cp -f "$TMP_SM"  "$TMP_DIR/prev_sm"
	cp -f "$TMP_V8"  "$TMP_DIR/prev_v8"
	cp -f "$ERROR_FILE" "$TMP_DIR/prev_error"
	exit 1
    fi
    else
  exit 0
fi
