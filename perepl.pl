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

    return ($self->{changed}?"*":"") 
         . $self->{name}
         . (stat $self->{name}? "" : "<new>");
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

sub File::modify {
    my ($self, $val) = @_;

    $self->{content} = $val;
    $self->{changed} = 1;

    return $self;
}

sub File::save {
    my ($self) = @_;

    open(my $fh, ">$self->{name}")
        or die "Can't open '$self->{name}' for writing";

    print $fh $self->{content};

    delete $self->{changed};

    close($fh);
}

sub Selection::new {
    my ($class, $base, $pattern) = @_;

    return bless {
        base => $base,
        pattern => $pattern,

        back => "",
        match => "",
        front => $base->read()
    } => $class;
}

sub Selection::next {
    my ($self) = @_;

    if ($self->{front} && $self->{front} =~ $self->{pattern}) {
        $self->{back} .= $self->{match} . $`;
        $self->{match} = $&;
        $self->{front} = $';
    } else {
        $self->{back} = $self->{match} . $self->{front};
        $self->{match} = $self->{front} = "";
    }

    return $self;
}

sub Selection::tag {
    my ($self) = @_;

    if(defined($self->{match})) {
        $self->{back} =~ m/\n([^\n]*)$/s;
        my $backself = $1 // "";

        $self->{front} =~ m/^([^\n]*)/s;
        my $frontself = $1 // "";

        return "'$backself<$self->{match}>$frontself'";
    }

    return "end";
}

sub Selection::extend {
    my ($self, $pattern) = @_;

    if($self->{front} =~ $pattern) {
        $self->{match} .= $` . $&;
        $self->{front} = $';
    }
}

sub Selection::modify {
    my ($self, $val) = @_;

    $self->{match} = $val;
    return $self;
}

sub Selection::read {
    my ($self) = @_;

    return $self->{match};
}

sub Selection::apply {
    my ($self) = @_;

    $self->{base}->modify($self->{back} . $self->{match} . $self->{front});
    return $self->{base};
}

my $CF;
sub edit {
    my ($name) = @_;

    return $CF = File->new($name);
}

sub sel {
    my ($selector) = @_;

    die "You must open a file first (use edit)"
        unless defined $CF;

    $CF = new Selection($CF, $selector)->next();
    return $CF->tag();
}

sub apply {
    die "You must open a file first (use edit)"
        unless defined $CF;
    
    $CF = $CF->apply();
    return $CF->tag();
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
    if(defined $CF) {
        return "\n\$CF=" . $CF->tag() . "> ";
    } else {
        return "\n> ";
    }
}

while(defined readeval(main_prompt())) {
}
