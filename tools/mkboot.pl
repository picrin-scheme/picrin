#!/usr/bin/perl

use strict;

# The maximum length of a string literal is 509 characters in C89.
# That is why the boot_rom is split into short strings.
my $chunk = 80;

sub print_escape_char($) {
    my $c = shift;
    if ($c eq "\n") {
        print "\\", "n";
    } elsif (($c eq "\\") || ($c eq '"')) {
        print "\\", $c;
    } else {
        print $c;
    }
}

print <<EOL;
#include "picrin.h"
#include "picrin/extra.h"

static const char boot_rom[][$chunk] = {
EOL
print "\"";
my $len = 0;
while (read(STDIN, my $c, 1)) {
    if ($len && ($len % $chunk == 0)) { print "\",\n\""; }
    print_escape_char($c);
    $len++;
}
if ($!) { die "read error"; }
print <<EOL;
",
};

void
pic_boot(pic_state *pic)
{
  pic_load_cstr(pic, &boot_rom[0][0]);
}
EOL
