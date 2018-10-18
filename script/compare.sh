#!/bin/sh

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

wat2wasm "$WAT_FILE" -o "$WASM_FILE"

node ../javascript/convert.js "$WASM_FILE" > "$JS_FILE"

# eshost -h Cha*,Sp*,Ja*,V8* -u "$JS_FILE"

wasm "$WASM_FILE" -e "(invoke \"aexp\")" | sed "s/\([0-9]\+\).*/\1/" > $TMP_REF
ch "$JS_FILE" > $TMP_CH
v8 "$JS_FILE" > $TMP_V8
sm "$JS_FILE" > $TMP_SM
jsc "$JS_FILE" > $TMP_JSC

REF_CH="$(cmp $TMP_REF $TMP_CH)"
CH_V8="$(cmp $TMP_CH $TMP_V8)"
V8_SM="$(cmp $TMP_V8 $TMP_SM)"
SM_JSC="$(cmp $TMP_SM $TMP_JSC)"

if [ "$REF_CH" = "$CH_V8" ] && [ "$CH_V8" = "$V8_SM" ] #&& [ "$V8_SM" = "$SM_JSC" ]
then
    rm $TMP_DIR/tmp_*
    rmdir "$TMP_DIR"
    exit 0
else
    exit 1
fi
