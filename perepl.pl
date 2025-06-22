#!/usr/bin/env perl

use strict;
use warnings;
use Term::ReadLine;
use Data::Dumper;

my $test_start;
sub test(&) {
    my ($fun) = @_;

    my ($pack, $fname, $line) = caller(0);

    $test_start = "$fname:$line";

    $fun->();
}

sub assert {
    my ($exp,$ref) = @_;

    my $ans = eval $exp;
    print "in test $test_start: $@" if $@; 

    if(defined $ref) {
        print "******\ntest $test_start failed, eval '$exp':\n$ans\nIS NOT EQUAL TO:\n$ref\n******\n"
            unless $ref eq $ans;
    } else {
        print "******\ntest $test_start failed, eval '$exp' returned false\n******\n"
            unless $ans;
    }
}

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

sub File::append {
    my ($self, $txt) = @_;

    $self->read()
        unless(defined($self->{content}));

    $self->{content} .= $txt;
    $self->{changed} = 1;

    return $self;
}

sub File::modify {
    my ($self, $val) = @_;

    $self->{content} = $val;
    $self->{changed} = 1;

    return $self;
}

sub File::apply {
    my ($self) = @_;

    if($self->{changed}) {
        open(my $fh, ">$self->{name}")
            or die "Can't open '$self->{name}' for writing";

        print $fh $self->{content};

        delete $self->{changed};

        close($fh);
    }
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

        if(defined $self->{mapper}) {
            $self->{mapper}->($self);
        }

        $self->next()
            if($self->{filter} && !$self->{filter}->($self));
    } else {
        $self->{back} = $self->{match} . $self->{front};
        $self->{match} = $self->{front} = "";
    }

    return $self;
}

sub Selection::map {
    my ($self, $mapper) = @_;

    $self->{mapper} = $mapper;

    if($self->{match}) {
        $mapper->($self);
    }

    return $self;
}

sub Selection::filter {
    my ($self, $filter) = @_;

    $self->{filter} = $filter;
    $self->next()
        unless $self->{filter}->($self);
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

sub Selection::append {
    my ($self, $txt) = @_;

    $self->{match} .= $txt;
    return $self;
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

sub LevelScanner::new {
    my ($class, %params) = @_;

    $params{openers} = ['\(', '\{', '\[']
        unless defined $params{openers};
    $params{closers} = ['\)', '\}', '\]']
        unless defined $params{closers};
    $params{ignorables} = ['"(\\\\.|[^"])*"',
                           "'(\\\\.|[^'])*'"]
        unless defined $params{ignorables};
    $params{line_limiters} = [',', ';'];

    for my $type (qw/openers closers ignorables line_limiters/) {
        $params{$type} = join("|", @{$params{$type}});
    }

    return bless { %params } => $class;
}

sub LevelScanner::level_change {
    my ($self, $text) = @_;

    return 0 if $text =~ m/^\s*$/;

    my $initial = 0;
    my $dlev = 0;
    my $terminated = 0;
    while($text =~ m/$self->{openers}|$self->{closers}|$self->{ignorables}|$self->{line_limiters}/g) {
        my ($back, $match, $front) = ($`, $&, $');

        next if ($match =~ m/$self->{ignorables}/);

        if($match =~ m/$self->{openers}/) {
            $terminated = 1 if $front =~ m/^\s*$/;

            $dlev++;
        } elsif ($match =~ m/$self->{closers}/) {
            if($back =~ m/^(\s|$self->{closers})*$/) {
                $terminated = 1 if $front =~ m/^\s*$/ && $match eq '}';
                $initial--;
            } else {
                $dlev--;
            }
        } elsif ($match =~ m/$self->{line_limiters}/ && $front =~ m/^\s*$/) {
            $terminated = 1;
        } elsif ($match =~ m/$self->{ignorables}/) {
            print "ignorable: $match\n";
        }
    }

    return ($initial, $dlev, $terminated);
}

my $default_level_scanner = new LevelScanner();

sub Selection::indent {
    my ($self, $ts) = @_;

    $ts //= 4;

    $self->{match} =~ m/^(\s+)/;
    my $level = $1? length($1) : 0;
    my $terminated = 1;
    my @lines = split(/\n/, $self->{match}); 
    for(my $ln = 0; $ln < @lines; $ln++) {
        $lines[$ln] =~ s/^\s+//;

        my ($initial,$dlev,$term) = $default_level_scanner->level_change($lines[$ln]);
        next unless defined $dlev;

        $level += $initial * $ts;
        $level -= $ts
            if(!$terminated && $initial < 0);

        $lines[$ln] = (" " x $level) . $lines[$ln]
            if $level > 0;

        $level += $ts * $dlev;
        $level += $ts * ($terminated - $term)
            unless(!$terminated && $initial < 0);

        $terminated = $term;
    }

    $self->{match} = join("\n", @lines);
    return $self->{match};
}

test {
    sub test_indent {
        my ($text, $result) = @_;

        $text =~ s/'/\\'/g;
        assert("(bless {match=>'$text'} => 'Selection')->indent()", $result);
    }

    test_indent("  foo\nbar, baz;\n", 
                "  foo\n      bar, baz;");
    test_indent("sub foo {\n my \$bar = \$_;\n     }",
                "sub foo {\n    my \$bar = \$_;\n}");
    test_indent("sub foo {\n my \$bar = '\\}';\nassert_eq(\$foo,\n'\\}'\n);\n}",
                "sub foo {\n    my \$bar = '\\}';\n    assert_eq(\$foo,\n        '\\}'\n    );\n}");
    test_indent('sub File::apply {
my ($self) = @_;

if($self->{changed}) {
       open(my $fh, ">$self->{name}")
    or die "Can\'t open \'$self->{name}\' for writing";

print $fh $self->{content};

           delete $self->{changed};
  }

        close($fh);
        }',
        'sub File::apply {
    my ($self) = @_;

    if($self->{changed}) {
        open(my $fh, ">$self->{name}")
            or die "Can\'t open \'$self->{name}\' for writing";

        print $fh $self->{content};

        delete $self->{changed};
    }

    close($fh);
}')
};

sub Selection::read {
    my ($self) = @_;

    return $self->{match};
}

sub Selection::apply {
    my ($self) = @_;

    $self->{base}->modify($self->{back} . $self->{match} . $self->{front});
    return $self->{base};
}

sub block {
    my ($sel) = @_;

    if($sel->{front} =~ m#\{#) {
        my $nb = $` . $&;
        my $rest = $';

        my $lev = 1;
        while($lev > 0 && $rest =~ m/"[^"]*"|'[^']*'|\{|\}/) {
            $nb .= $` . $&;
            $rest = $';
            if($& eq "{") {
                $lev++;
            } elsif ($& eq "}") {
                $lev--;
            }
        }

        $sel->{match} .= $nb;
        $sel->{front} = $rest;
    }

    return $sel;
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
    return $CF;
}

sub funs {
    my ($filter) = @_;

    sel('sub\s*[\w:]*');
    $CF->map(\&block);
    $CF->filter(sub { $_[0]->{match} =~ m/sub\s*$filter/ })
        if defined $filter;
}

sub append {
    if(ref($CF) eq "Selection") {
        my $txt = "";
        while(defined(my $line = $reader->readline('"'))) {
            $txt .= "$line\n";
        }
        
        $CF->{match} .= $txt;
    }
}

my $APPEND_AUTOINDENT = 1;

sub apply {
    my ($autoindent) = @_;

    $autoindent //= $APPEND_AUTOINDENT;

    return 0 unless defined $CF;
    
    $CF = $CF->apply();
    $CF->indent() if ($autoindent && ref($CF) eq "Selection");
    return $CF->tag() if defined($CF);
}

sub apply_ask {
    if(ref($CF) eq "File" && $CF->{changed}) {
        print "There are unsaved changes in '$CF->{name}', do you want to save it? [y/n/c] ";
        my $ans = <STDIN>;

        return 1 unless $ans && $ans =~ m/\S+/;

        if(lc($&) eq "y") {
            apply();
            return 0;
        } elsif (lc($&) eq "n") {
            return 0;
        }

        return 1;
    }

    apply();
}

sub nextm {
    if($CF) {
        $CF->next();
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
    if(defined $CF) {
        return "\n\$CF=" . $CF->tag() . "> ";
    } else {
        return "\n> ";
    }
}

while(defined readeval(main_prompt()) || apply_ask()) {
}
