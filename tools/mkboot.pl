#!/usr/bin/perl

use strict;

sub constant($$) {
    # The maximum length of a string literal is 509 characters in C89.
    # That is why src is split into short strings.
    my ($var, $src) = @_;
    print "static const char ${var}[][80] = {\n";
    my @lines = $src =~ /.{0,80}/gs;
    foreach (@lines) {
        s/\\/\\\\/g;
        s/"/\\"/g;
        s/\n/\\n/g;
        print "\"$_\",\n";
    }
    print "};\n\n";
}

local $/ = undef;

print <<EOL;
#include "picrin.h"
#include "picrin/extra.h"

EOL
open(IN, "piclib/boot.scm");
constant("boot_rom", <IN>);
close(IN);
print <<EOL;

#if PIC_USE_LIBRARY
EOL
open(IN, "piclib/library.scm");
constant("boot_library_rom", <IN>);
close(IN);
print <<EOL;
#endif

void
pic_boot(pic_state *pic)
{
  pic_load_cstr(pic, &boot_rom[0][0]);
#if PIC_USE_LIBRARY
  pic_load_cstr(pic, &boot_library_rom[0][0]);
#endif
}
EOL
