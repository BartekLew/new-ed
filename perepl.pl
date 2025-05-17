#!/usr/bin/env perl

use strict;
use warnings;
use Term::ReadLine;
use Data::Dumper;

my $reader = Term::ReadLine->new('SmartPrompt');
my $out = $reader->OUT || \*STDOUT;

sub File::new {
    my ($class, $filename) = @_;

    return bless { name => $filename } 
                 => $class;
}

sub File::tag {
    my ($self) = @_;

    if(stat $self->{name}) {
        return "$self->{name}"
    } else {
        return "$self->{name}<new>";
    }
}

sub File::read {
    my ($self) = @_;

    unless(defined $self->{content}) {
        open(my $fh, "$self->{name}") 
            or die "Can't open file '$self->{name}' for reading";

        $self->{content} = join("", <$fh>);
        
        close($fh);
    };

    return $self->{content};

}

sub Selection::new {
    my ($class, $back, $match, $front) = @_;

    return bless {
        back => $back,
        match => $match,
        front => $front
    } => $class;
}

sub Selection::tag {
    my ($self) = @_;

    $self->{back} =~ m/\n([^\n]*)$/s;
    my $backself = $1 // "";

    $self->{front} =~ m/^([^\n]*)/s;
    my $frontself = $1 // "";

    return "'$backself<$self->{match}>$frontself'";
}

sub Selection::extend {
    my ($self, $pattern) = @_;

    if($self->{front} =~ $pattern) {
        $self->{match} .= $` . $&;
        $self->{front} = $';
    }
}

my $CURRENT_FILE;
sub edit {
    my ($name) = @_;

    $CURRENT_FILE = File->new($name);
    return $CURRENT_FILE->tag();
}

sub sel {
    my ($selector) = @_;

    die "You must open a file first (use edit)"
        unless defined $CURRENT_FILE;

    my $done = "";
    my $rest = $CURRENT_FILE->read();
    while($rest && $rest =~ $selector) {
        my $ctx = Selection->new($`, $&, $');
        
        print "\nmatched " . $ctx->tag() . "\n";

        while(defined readeval("$ctx->{match}>", $ctx)) {}

        $done .= $ctx->{back};
        $rest = $ctx->{front};
    }
}

sub readeval {
    my($prompt, $ctx) = @_;

    $ctx //= $_;

    my $line = $reader->readline($prompt);

    return unless defined $line;
    return "" unless $line;
    
    $reader->addhistory($line)
        if $line =~ m/S/;

    local $_ = $ctx;
    my $ans = eval $line;
    if($@) {
        print STDERR $@;
        return $@;
    } elsif ($ans) {
        print "\$_ = $ans\n";
        $_ = $ans;
    }

    return $ans;
}

sub main_prompt {
    if(defined $CURRENT_FILE) {
        return "\n" . $CURRENT_FILE->tag() . "> ";
    } else {
        return "\n> ";
    }
}

while(defined readeval(main_prompt())) {
}
