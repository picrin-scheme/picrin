#!/usr/bin/perl

@files = (
    'piclib/built-in.scm',
    'piclib/srfi/1.scm',
    'piclib/srfi/26.scm',
    'piclib/srfi/95.scm'
    );

print <<EOL;
#include "picrin.h"
#include "picrin/error.h"

EOL

foreach my $file (@files) {
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

foreach my $file (@files) {
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
    my ($_) = @_;
    s/\.scm$//g;
    s/[\/-]/_/g;
    $_;
}
