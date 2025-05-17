#!/usr/bin/env perl

use strict;
use warnings;

my $PROMPT = "\n> ";

sub readcmd {
    my $cmd = "";
    
    while(my $line = <STDIN>) {
        if($line =~ m/^(.*)\\$/) {
           $cmd .= "$1 ";
        } else {
           $cmd .= $line;
           last;
        }
    }

    return $cmd;
}

while(1) {
    print $PROMPT;
    my $line = readcmd();

    last unless $line;

    eval $line;
    if($@) {
        print STDERR $@;
    }
}
