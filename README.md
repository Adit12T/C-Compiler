# C Compiler (pcc3)

[![CI](https://github.com/Adit12T/C-Compiler/actions/workflows/ci.yml/badge.svg)](https://github.com/Adit12T/C-Compiler/actions/workflows/ci.yml)

A compiler for a subset of C that produces **32-bit x86 assembly**, written in C with **flex** and
**bison**. I built it in three stages for the compiler-construction course in my MS at the University
of South Carolina (2017–2018), on top of a skeleton supplied by the course.

## The three stages

| Folder | Stage | What it adds |
|---|---|---|
| `proj1/` | Declarations | Parses global declarations (pointers, arrays, functions and their parameters), builds their types, enters them in the symbol table and reserves storage |
| `proj2/` | Expressions | Builds syntax trees for expressions (arithmetic, comparison, assignment, function calls, type conversions) and generates code for them |
| `proj3/` | Statements | Adds control flow: `if`/`else`, `while`, `do`/`while`, `for`, `break`, `continue`, `return` and `switch` |

Each folder is a complete compiler, so the stages can be compared side by side. The parts I wrote are
mainly the grammar actions (`gram.y`), the syntax trees (`tree.c`, `tree.h`) and code generation
(`encode.c`); the symbol table, type module, message handling and x86 back end come from the course
skeleton.

## Build and run

Needs `gcc`, `flex`, `bison` and, to run the output, 32-bit libraries (`gcc-multilib` on Ubuntu).

```bash
make                            # builds proj1/pcc3, proj2/pcc3, proj3/pcc3
./proj3/pcc3 < tests/stage3/loops.c > loops.s
gcc -m32 -no-pie loops.s -o loops && ./loops
./proj1/pcc3 -d < tests/stage1/decls.c   # dump the symbol table
make test                       # run every test program
```

## Tests

`tests/run.sh` runs each program in `tests/stage1`–`stage3` through its stage. Stage 1 tests check
the types in the symbol-table dump; stage 2 and 3 tests are compiled, linked, run, and their output
and exit code compared with the expected files. CI runs them on every push.

## 2026 maintenance

The code no longer built on current toolchains. I fixed that without changing what it does:

- `yylval` was defined twice (scanner and parser), which GCC 10 and later reject; the scanner now
  declares it `extern`.
- `encode.h` reused `tree.h`'s include guard, so its function prototype was silently skipped.
- Two calls passed `NULL` where an enum was expected, which GCC 14 treats as an error; they now pass
  the matching enum values (`NO_QUAL`, `PROTOTYPE`), which have the same value.
- Added the missing `<string.h>` include and a prototype for `is_ptr`.
- GCC 15 compiles C23 by default, where an empty parameter list means "no arguments"; the skeleton's
  callback types rely on the older meaning, so the Makefiles now select `-std=gnu11`.

## Known limitations

This is coursework, not a production compiler:

- Variables declared inside a function are stored as globals, so recursion doesn't work.
- `double` constants inside function bodies stop the compiler with an internal error.
- Only the 32-bit x86 target is supported.

## License

GNU General Public License v3; see [LICENSE](LICENSE). The course skeleton belongs to its authors.
