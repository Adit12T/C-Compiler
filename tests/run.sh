#!/usr/bin/env bash
# Runs every test program through its stage of the compiler.
#  stage1: dumps the symbol table and checks each line of NAME.expected appears in it.
#  stage2/3: compiles to 32-bit x86 assembly, links with gcc -m32, runs the program and compares
#            its output with NAME.expected and/or its exit code with NAME.exitcode.
set -u
cd "$(dirname "$0")/.."
pass=0; fail=0; work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
report() { if [ "$1" = ok ]; then pass=$((pass+1)); echo "PASS $2"; else fail=$((fail+1)); echo "FAIL $2: $3"; fi; }

for src in tests/stage1/*.c; do
  name=${src%.c}
  if ! ./proj1/pcc3 -d < "$src" > "$work/dump" 2>&1; then report bad "$src" "compiler exited non-zero"; continue; fi
  missing=$(grep -vxF -f <(sed 's/^[[:space:]]*//' "$work/dump" | sed -n 's/^type = //p') "$name.expected")
  if [ -z "$missing" ]; then report ok "$src"; else report bad "$src" "missing: $missing"; fi
done

for stage in 2 3; do
  for src in tests/stage$stage/*.c; do
    name=${src%.c}
    if ! ./proj$stage/pcc3 < "$src" > "$work/out.s" 2> "$work/err"; then report bad "$src" "$(head -1 "$work/err")"; continue; fi
    if ! gcc -m32 -no-pie "$work/out.s" -o "$work/prog" 2> "$work/err"; then report bad "$src" "link: $(head -1 "$work/err")"; continue; fi
    "$work/prog" > "$work/actual"; code=$?
    if [ -f "$name.expected" ] && ! diff -u "$name.expected" "$work/actual" > "$work/diff"; then report bad "$src" "output differs"; cat "$work/diff"; continue; fi
    if [ -f "$name.exitcode" ] && [ "$code" != "$(cat "$name.exitcode")" ]; then report bad "$src" "exit code $code"; continue; fi
    report ok "$src"
  done
done
echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
