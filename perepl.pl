#!/usr/bin/env perl

use strict;
use warnings;
use Term::ReadLine;

my $PROMPT = "\n> ";
my $reader = Term::ReadLine->new('SmartPrompt');
my $out = $reader->OUT || \*STDOUT;

while(1) {
    print $PROMPT;
    my $line = $reader->readline($PROMPT);

    last unless defined $line;

    $reader->addhistory($line)
        if $line =~ m/S/;

    my $ans = eval $line;
    if($@) {
        print STDERR $@;
    } elsif ($ans) {
        print "\$_ = $ans";
        $_ = $ans;
    }
}
