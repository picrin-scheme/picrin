find_package(FLEX REQUIRED)
execute_process(COMMAND flex scan.l WORKING_DIRECTORY src)

add_library(picrin SHARED
  src/blob.c src/bool.c src/char.c src/codegen.c src/cont.c src/error.c
  src/file.c src/gc.c src/init.c src/lex.yy.c src/lib.c src/load.c src/macro.c
  src/number.c src/pair.c src/port.c src/proc.c src/read.c src/state.c
  src/string.c src/symbol.c src/system.c src/time.c src/var.c src/vector.c
  src/vm.c src/write.c)
target_link_libraries(picrin m xfile)
