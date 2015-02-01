#!/usr/bin/perl

use strict;
use File::Basename qw/basename dirname/;

print <<EOL;
/**
 *                                !!NOTICE!!
 * This file was automatically generated by mkloader.pl, and includes all of
 * the prelude files required by Picrin. PLEASE DO NOT EDIT THIS FILE, changes
 * will be overwritten the next time the script runs.
 */

#include "picrin.h"
#include "picrin/error.h"
#include "picrin/port.h"

EOL

foreach my $file (@ARGV) {
    my $var = &escape_v($file);
    print "static const char ${var}[][80] = {\n";

    open IN, $file;
    local $/ = undef;
    my $src = <IN>;
    close IN;

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
void
pic_load_piclib(pic_state *pic)
{
EOL

foreach my $file (@ARGV) {
    print "  pic_try {\n";
    my $var = &escape_v($file);
    my $basename = basename($file);
    my $dirname = basename(dirname($file));
    print "    pic_load_cstr(pic, &${var}[0][0]);\n";
    print<<EOL
  }
  pic_catch {
    /* error! */
    xfprintf(pic->xSTDERR->file, "fatal error: failure in loading $dirname/$basename\\n");
    xfprintf(pic->xSTDERR->file, pic_errmsg(pic));
    pic_panic(pic, "failure in loading $dirname/$basename");
  }
EOL
}

print <<EOL;

#if DEBUG
  puts("successfully loaded stdlib");
#endif
}
EOL

sub escape_v {
    ($_) = @_;
    s/\.scm$//g;
    s/[^[A-Za-z0-9_]/_/g;
    "piclib_src_" . $_;
}
