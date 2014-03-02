find_package(FLEX REQUIRED)
flex_target(scan src/scan.l src/lex.yy.c COMPILE_FLAGS --header-file="src/lex.yy.h")

add_library(picrin SHARED
  src/blob.c src/bool.c src/char.c src/codegen.c src/cont.c src/error.c
  src/file.c src/gc.c src/init.c src/lib.c src/load.c src/macro.c
  src/number.c src/pair.c src/port.c src/proc.c src/read.c src/state.c
  src/string.c src/symbol.c src/system.c src/time.c src/var.c src/vector.c
  src/vm.c src/write.c ${PROJECT_SOURCE_DIR}/src/lex.yy.c)
target_link_libraries(picrin m xfile)
