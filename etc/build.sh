#!/usr/bin/bash
project_root=$(dirname $(pwd))
for f in ${project_root}/src/*.c ${project_root}/extlib/benz/*.c ${project_root}/contrib/**/*.c ;do
    gcc -c -O2 -pg -std=c99  -I${project_root}/extlib/benz/include $f -o `basename $f`.o &
done
wait
gcc  *.o -O2 -I${project_root}/src -I${project_root}/extlib/benz/include -std=c99 -pg -lm -lreadline -o picrin
rm *.o
