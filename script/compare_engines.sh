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

TMP_REF="$TMP_DIR/tmp_ref"
TMP_CH="$TMP_DIR/tmp_ch"
TMP_JSC="$TMP_DIR/tmp_jsc"
TMP_SM="$TMP_DIR/tmp_sm"
TMP_V8="$TMP_DIR/tmp_v8"

TMP_CH_JSC="$TMP_DIR/tmp_cmp_ch_jsc"
TMP_JSC_SM="$TMP_DIR/tmp_cmp_jsc_sm"
TMP_SM_V8="$TMP_DIR/tmp_cmp_sm_v8"

ERROR_FILE="$TMP_DIR/error"

if [ "$#" -eq 2 ]
then
    STAT_DIR="stat"
    STAT_WAT2WASM="$STAT_DIR/$2wat2wasm_time"

    /usr/bin/time -f "%E" -a -o "$STAT_WAT2WASM" wat2wasm "$WAT_FILE" -o "$WASM_FILE" 2> "$ERROR_FILE"

else
    wat2wasm "$WAT_FILE" -o "$WASM_FILE" 2> "$ERROR_FILE"
fi

ERROR=$?
if [ "$ERROR" != 0 ]
then
    exit "$ERROR"
fi

if [ "$#" -eq 2 ]
then
    STAT_NODE="$STAT_DIR/$2node_time"

    /usr/bin/time -f "%E" -a -o "$STAT_NODE" node ../javascript/convert.js "$WASM_FILE" "ch"  > "$JS_CH_FILE"  2> "$ERROR_FILE"
    /usr/bin/time -f "%E" -a -o "$STAT_NODE" node ../javascript/convert.js "$WASM_FILE" "jsc" > "$JS_JSC_FILE" 2> "$ERROR_FILE"
    /usr/bin/time -f "%E" -a -o "$STAT_NODE" node ../javascript/convert.js "$WASM_FILE" "sm"  > "$JS_SM_FILE"  2> "$ERROR_FILE"
    /usr/bin/time -f "%E" -a -o "$STAT_NODE" node ../javascript/convert.js "$WASM_FILE" "v8"  > "$JS_V8_FILE"  2> "$ERROR_FILE"

else
    node ../javascript/convert.js "$WASM_FILE" "ch"  > "$JS_CH_FILE"  2> "$ERROR_FILE"
    node ../javascript/convert.js "$WASM_FILE" "jsc" > "$JS_JSC_FILE" 2> "$ERROR_FILE"
    node ../javascript/convert.js "$WASM_FILE" "sm"  > "$JS_SM_FILE"  2> "$ERROR_FILE"
    node ../javascript/convert.js "$WASM_FILE" "v8"  > "$JS_V8_FILE"  2> "$ERROR_FILE"
fi

ERROR=$?
if [ "$ERROR" != 0 ]
then
    exit "$ERROR"
fi

if [ "$#" -eq 2 ]
then
    STAT_CH_TIME="$STAT_DIR/$2ch_time"
    STAT_JSC_TIME="$STAT_DIR/$2jsc_time"
    STAT_SM_TIME="$STAT_DIR/$2sm_time"
    STAT_V8_TIME="$STAT_DIR/$2v8_time"
    STAT_BACKTRACK="$STAT_DIR/$2backtrack"

    /usr/bin/time -f "%E" -a -o "$STAT_CH_TIME" timeout  10 ch "$JS_CH_FILE" > $TMP_CH 2>&1
    /usr/bin/time -f "%E" -a -o "$STAT_JSC_TIME" timeout 10 jsc "$JS_JSC_FILE" > $TMP_JSC 2>&1
    /usr/bin/time -f "%E" -a -o "$STAT_SM_TIME" timeout  10 sm "$JS_SM_FILE" > $TMP_SM 2>&1
    /usr/bin/time -f "%E" -a -o "$STAT_V8_TIME" timeout  10 v8 "$JS_V8_FILE" > $TMP_V8 2>&1

    if [ -f "stat/backtrack" ]
    then
        wc -l "stat/backtrack" | sed "s/\([0-9]*\).*/\1/" >> "$STAT_BACKTRACK"
        rm "stat/backtrack"
    else
        echo 0 >> "$STAT_BACKTRACK"
    fi
else 

    timeout 10 ch "$JS_CH_FILE" > "$TMP_CH" 2>&1
    timeout 10 jsc "$JS_JSC_FILE" > "$TMP_JSC" 2>&1
    timeout 10 sm "$JS_SM_FILE" > "$TMP_SM" 2>&1
    timeout 10 v8 "$JS_V8_FILE" > "$TMP_V8" 2>&1

fi

cmp -n 5000 $TMP_CH $TMP_JSC > $TMP_CH_JSC 2>&1
cmp -n 5000 $TMP_JSC $TMP_SM > $TMP_JSC_SM 2>&1
cmp -n 5000 $TMP_SM $TMP_V8 > $TMP_SM_V8 2>&1

if [ -s "$TMP_CH_JSC" ] || [ -s "$TMP_JSC_SM" ] || [ -s "$TMP_SM_V8" ]
then
  # Save diff result, to avoid last shrinking step overwriting it
  cp -f "$WAT_FILE"  "$TMP_DIR/prev.wat"
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
else
  exit 0
fi
