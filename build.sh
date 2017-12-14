ocamlc -c -g -no-alias-deps -w -40 -I +compiler-libs ocamlcommon.cma refmt_api.ml
ocamlc -c -pp "refmt --print binary" -I +compiler-libs -g -impl migrate.re
ocamlc -I +compiler-libs ocamlcommon.cma unix.cma refmt_api.cmo migrate.cmo -o migrate.exe
./migrate.exe component.re

# ocamlc -c -pp "refmt --print binary" -I +compiler-libs -g -impl migrate.re
# ocamlc -I +compiler-libs ocamlcommon.cma unix.cma migrate.cmo -o migrate.exe
# ./migrate.exe component.re

# this one compiles everything in a single shot
# ocamlc -pp "refmt --print binary" -g -no-alias-deps -w -40 \
#   -I +compiler-libs ocamlcommon.cma ./refmt_api.ml \
#   -impl migrate.re  -o migrate.exe

# ocamlc -I +compiler-libs ocamlcommon.cma -pp "refmt --print binary" -impl migrate.re -o migrate.exe
# bsc -bs-syntax-only -bs-no-builtin-ppx-ml -pp "refmt --print binary" -dsource -ppx ./migrate.exe -impl component.re 2>&1 | refmt --parse ml
