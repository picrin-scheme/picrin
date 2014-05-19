#!/usr/bin/perl

use strict;

print <<EOL;
#include "picrin.h"
#include "picrin/error.h"

EOL

foreach my $file (@ARGV) {
    my $var = &escape_v($file);
    print "const char *$var =\n";

    open IN, $file;
    while (<IN>) {
        chomp;
        s/\\/\\\\/g;
        s/"/\\"/g;
        print "\"$_\\n\"\n";
    }
    print ";\n\n";
}
close IN;

print <<EOL;
void
pic_load_piclib(pic_state *pic)
{
  pic_try {
EOL

foreach my $file (@ARGV) {
    my $var = &escape_v($file);
    print "    pic_load_cstr(pic, $var);\n";
}

print <<EOL;
  }
  pic_catch {
    /* error! */
    fputs("fatal error: failure in loading built-in.scm\\n", stderr);
    fputs(pic_errmsg(pic), stderr);
    abort();
  }

#if DEBUG
  puts("successfully loaded stdlib");
#endif
}
EOL

sub escape_v {
    ($_) = @_;
    s/\.scm$//g;
    s/[^[A-Za-z0-9_]/_/g;
    $_;
}
