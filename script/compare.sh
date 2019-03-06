#!/bin/bash

if [ "$#" -ne 1 ] || ! [ -f "$1" ]; then
  echo "Usage: $0 FILE" >&2
  exit 1
fi

TMP_DIR="tmp"

if ! [ -d "$TMP_DIR" ]
then
    mkdir "$TMP_DIR"
fi

WAT_FILE="$1"
WASM_FILE="$TMP_DIR/tmp_generated.wasm"
JS_FILE="$TMP_DIR/tmp_generated.js"

TMP_REF="$TMP_DIR/tmp_ref"
TMP_CH="$TMP_DIR/tmp_ch"
TMP_V8="$TMP_DIR/tmp_v8"
TMP_SM="$TMP_DIR/tmp_sm"
TMP_JSC="$TMP_DIR/tmp_jsc"

ERROR_FILE="$TMP_DIR/error"

wat2wasm "$WAT_FILE" -o "$WASM_FILE" 2> "$ERROR_FILE"
ERROR=$?
if [ "$ERROR" != 0 ]
then
    exit "$ERROR"
fi

node ../javascript/convert.js "$WASM_FILE" > "$JS_FILE" 2> "$ERROR_FILE"
ERROR=$?
if [ "$ERROR" != 0 ]
then
    exit "$ERROR"
fi

timeout 2 wasm "$WASM_FILE" -e "(invoke \"aexp\")" 2> >(sed "s/.*\(integer\)/--> \1/") | sed "s/\(-\?[0-9]\+\).*/--> \1/" > $TMP_REF
timeout 2 ch "$JS_FILE" > $TMP_CH 2>&1
timeout 2 v8 "$JS_FILE" > $TMP_V8 2>&1
timeout 2 sm "$JS_FILE" > $TMP_SM 2>&1
timeout 2 jsc "$JS_FILE" > $TMP_JSC 2>&1

cmp -s $TMP_REF $TMP_CH
REF_CH=$?

cmp -s $TMP_CH $TMP_V8
CH_V8=$?

cmp -s $TMP_V8 $TMP_SM
V8_SM=$?

cmp -s $TMP_SM $TMP_JSC
SM_JSC=$?

#if [ "$REF_CH" != 1 ] && [ "$REF_CH" = "$CH_V8" ] && [ "$CH_V8" = "$V8_SM" ] && [ "$V8_SM" = "$SM_JSC" ]
if [ "$REF_CH" = "$CH_V8" ] && [ "$CH_V8" = "$V8_SM" ] && [ "$V8_SM" = "$SM_JSC" ]
then
    # rm $TMP_DIR/tmp_*
    # rmdir "$TMP_DIR"
    exit 0
else
    exit 1
fi
