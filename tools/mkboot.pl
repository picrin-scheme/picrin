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

print <<EOL;
#include "picrin.h"
#include "picrin/extra.h"

EOL
local $/ = undef;
constant("boot_rom", <STDIN>);
print <<EOL;

void
pic_boot(pic_state *pic)
{
  pic_load_cstr(pic, &boot_rom[0][0]);
}
EOL
