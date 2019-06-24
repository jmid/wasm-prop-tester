#!/bin/bash


TMP_DIR="tmp"

if ! [ -d "$TMP_DIR" ]
then
    mkdir "$TMP_DIR"
fi

WAT_FILE="../failed/failed_loop_stack.wat"
WASM_FILE="$TMP_DIR/tmp_module.wasm"
JS_CH_FILE="$TMP_DIR/tmp_ch.js"
JS_JSC_FILE="$TMP_DIR/tmp_jsc.js"
JS_SM_FILE="$TMP_DIR/tmp_sm.js"
JS_V8_FILE="$TMP_DIR/tmp_v8.js"

TMP_CH="$TMP_DIR/tmp_ch"
TMP_JSC="$TMP_DIR/tmp_jsc"
TMP_SM="$TMP_DIR/tmp_sm"
TMP_V8="$TMP_DIR/tmp_v8"

TMP_CH_STACK="$TMP_DIR/tmp_ch_stack"
TMP_JSC_STACK="$TMP_DIR/tmp_jsc_stack"
TMP_SM_STACK="$TMP_DIR/tmp_sm_stack"
TMP_V8_STACK="$TMP_DIR/tmp_v8_stack"

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


timeout 10 ch "$JS_CH_FILE" > $TMP_CH 2>&1
timeout 10 jsc "$JS_JSC_FILE" > $TMP_JSC 2>&1
timeout 10 sm "$JS_SM_FILE" > $TMP_SM 2>&1
timeout 10 v8 "$JS_V8_FILE" > $TMP_V8 2>&1

wc -l $TMP_CH  | sed "s/\([0-9]\+\) .*/ \1/" >> "$TMP_CH_STACK"
wc -l $TMP_JSC | sed "s/\([0-9]\+\) .*/ \1/" >> "$TMP_JSC_STACK"
wc -l $TMP_SM  | sed "s/\([0-9]\+\) .*/ \1/" >> "$TMP_SM_STACK"
wc -l $TMP_V8  | sed "s/\([0-9]\+\) .*/ \1/" >> "$TMP_V8_STACK"

exit 0
