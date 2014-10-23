#!/usr/bin/zsh
project_root=$(dirname $(pwd))
for f in ${project_root}/src/*.c ${project_root}/extlib/benz/*.c ${project_root}/contrib/**/*.c;do
    cc -c -O2 -pg -std=c99  -I${project_root}/extlib/benz/include $f -o `basename $f`.o &
done
wait
cc main.c.o $(find . -maxdepth 1 -name '*.o' | grep -v main.c.o | grep -v load_piclib.c.o | grep -v init_contrib.c.o) load_piclib.c.o init_contrib.c.o \
   -O2 -I${project_root}/extlib/benz/include -std=c99 -pg -lm -lreadline -o picrin
rm *.o
