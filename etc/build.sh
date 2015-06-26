#!/bin/sh
set -e
: ${GCC:=gcc}
project_root=$(cd $(dirname $0); cd ../; pwd)

for f in ${project_root}/src/*.c ${project_root}/extlib/benz/*.c $(find ${project_root}/contrib/* -name '*.c');do
    ${GCC} -c -O2 -pg -std=c99  -I${project_root}/extlib/benz/include $f -o `basename $f`.o &
done
wait
${GCC} main.c.o $(find . -maxdepth 1 -name '*.o' | grep -v main.c.o | grep -v load_piclib.c.o | grep -v init_contrib.c.o) load_piclib.c.o init_contrib.c.o \
   -O2 -I${project_root}/extlib/benz/include -std=c99 -pg -lm -lreadline -o picrin
rm *.o
