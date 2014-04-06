import re
import os
from collections import OrderedDict

outfn = 'src/load_piclib.c'

libs = [
    'piclib/built-in.scm',
    'piclib/srfi/1.scm',
    'piclib/srfi/26.scm',
    'piclib/srfi/95.scm',
]

if os.path.exists(outfn):
    os.remove(outfn)

piclibs = OrderedDict()
for lib in libs:
    vname = lib[:-4]
    vname = re.sub(r'/', r'_', vname)
    vname = re.sub(r'-', r'_', vname)
    piclibs[lib] = vname

def escape_scm(infn, outfn, vname):
    with open(outfn, 'a') as output:
        output.write('const char *{} =\n'.format(vname))
        with open(infn, 'r') as input:
            for line in input:
                output.write('"')
                line = line.strip('\n')
                line = re.sub(r'\\', r'\\\\', line)
                line = re.sub(r'"', r'\"', line)
                output.write(line)
                output.write('\\n"\n')
        output.write(';\n\n')

piclib_load_head = """
#include "picrin.h"
#include "picrin/error.h"
"""

piclib_load_tail = """
void
pic_load_piclib(pic_state *pic)
{{
  pic_try {{
    {}
  }}
  pic_catch {{
    /* error! */
    fputs("fatal error: failure in loading built-in.scm\\n", stderr);
    fputs(pic_errmsg(pic), stderr);
    abort();
  }}

#if DEBUG
    puts("successfully loaded stdlib");
#endif

}}
"""

def gen_piclib_load_c(outfn):
    with open(outfn, 'a') as f:
        f.write(piclib_load_head)
    loads = ""
    for infn,vname in piclibs.items():
        escape_scm(infn, outfn, vname)
        loads += "pic_load_cstr(pic, {});\n    ".format(vname)
    with open(outfn, 'a') as f:
        f.write('\n\n')
        f.write(piclib_load_tail.format(loads))

gen_piclib_load_c(outfn)
