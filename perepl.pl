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

sub Result::ok {
    my ($val) = @_;

    return bless {val => $val} => "Result";
}

sub Result::failed {
    my ($msg) = @_;

    return bless { err => $msg } => "Result";
}

sub Result::terminate {
    my ($exit_code) = @_;
    return bless { terminate => 0 } => "Result";
}

sub Result::map {
    my ($self, $fun) = @_;

    if(defined $self->{val}) {
        local $_ = $self->{val};
        $self->{val} = $fun->();
    }

    return $self;
}

sub Result::unwrap {
    my ($self) = @_;

    return $self->{val} if(defined $self->{val});
    
    die $self->{err} // "Unknow error"
        if defined($self->{err});

    exit($self->{terminate});
}

sub Result::unwrap_or {
    my ($self, $default) = @_;

    return $self->{val} if(defined $self->{val});
    exit($self->{terminate})
        if(defined($self->{terminate}));

    return $default->() if (ref($default) eq "CODE");
    return $default;
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

sub assert_eq {
    my ($str,$ref) = @_;

    if(defined $ref) {
        print "******\ntest $test_start failed :\n$str\nIS NOT EQUAL TO:\n$ref\n******\n"
            unless $ref eq $str;
    } else {
        print "******\ntest $test_start failed returned false\n******\n"
            unless $str;
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

    if($self->{content} ne $val) {
        $self->{content} = $val;
        $self->{changed} = 1;
    }

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

    return $self->{base};
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

    $self->{match} =~ m/^([ \t]+)/;
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
    test_indent('
sub File::apply {
my ($self) = @_;

if($self->{changed}) {
       open(my $fh, ">$self->{name}")
    or die "Can\'t open \'$self->{name}\' for writing";

print $fh $self->{content};

           delete $self->{changed};
  }

        close($fh);
        }',
        '
sub File::apply {
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

sub Scanner::new {
    my ($class, @transitions) = @_;

    my @regex = @transitions[grep { $_ % 2 == 0 } (0..$#transitions)];
    my @action = @transitions[grep { $_ % 2 == 1 } (0..$#transitions)];

    return bless { actions => \@action,
                   partial_regex => \@regex,
                   regex => join("|", map { "($_)" } @regex) }
        => $class;
}

sub Scanner::scan {
    my ($self, $text) = @_;

    return bless { scanner => $self,
                   level => 0,
                   back => "",
                   front => $text } 
        => 'Scanning';
}

sub Scanning::step {
    my ($self) = @_;

    my $s = $self->{scanner};
    if($self->{front} =~ m/$s->{regex}/) {
        my ($back, $match, $front) = ($`, $&, $');
        for(my $tid = 0; $tid < @{$s->{actions}}; $tid++) {
            if($match =~ m/$s->{partial_regex}->[$tid]/) {
                $self->{front} = $front;
                $self->{back} .= $back . $match;
                return $s->{actions}->[$tid]->($self, $back, $match, $front);
            } 
        }
        die "INTERNAL ERROR: matched '$match', but no TID found";
    }

    return undef;
}

sub Scanning::drain {
    my ($self) = @_;

    while(defined($self->step())) {}

    return $self->{level};
}

sub noop { return 1; }

sub expect_delimiter {
    my ($delimiter) = @_;

    return sub {
        my($ctx, $back, $match, $front) = @_;

        my $base_scanner = $ctx->{scanner};
        $ctx->{level}++;
        $ctx->{scanner} = new Scanner('\\.' => \&noop,
                                      $delimiter => sub {
                                          my ($ctx) = @_;
                                          $ctx->{level}--;
                                          $ctx->{scanner} = $base_scanner;
                                      });
    };
}
my $perl_scanner = new Scanner(
    '\b(m)([/!@#\$])' => sub { # regex
        my ($ctx, $back, $match, $front) = @_;

        my $delimiter = substr($match, 1);
        my $base_scanner = $ctx->{scanner};
        $ctx->{level}++;
        $ctx->{scanner} = new Scanner($delimiter => sub {
                                                    my ($ctx) = @_;
                                                    $ctx->{level}--;
                                                    $ctx->{scanner} = $base_scanner;
                                                });
        return 1;
    },
    '[\$@%]([a-zA-Z0-9_]+|[^{])' => \&noop, # variable
    '"' => expect_delimiter('"'), # double quote
    "'" => expect_delimiter("'"), #single quote
    "\{" => sub {
        my($ctx) = @_;
        $ctx->{level}++;
        return $ctx->{level};
    },
    "\}" => sub {
        my($ctx) = @_;
        $ctx->{level}--;

        return $ctx->{level};
    });

sub block {
    my ($sel) = @_;

    my $proc = $perl_scanner->scan($sel->{front});

    while(1) {
        my $ans = $proc->step();
        return unless defined $ans;

        if($ans <= 0) {
            $sel->{match} .= $proc->{back};
            $sel->{front} = $proc->{front};
            return $sel;
        }
    }
}

test {
    sub test_block {
        my($match, $front, $ref) = @_;

        my $sel = bless { front => $front, match => $match } => 'Selection';
        my $ans = block($sel);
        assert_eq($ans->{match}, $ref);
    }

    test_block("sub", " { \$' = '}'; } foobar baz", "sub { \$' = '}'; }");
    test_block("sub", ' { $ans = $` + "}"; } foobar baz', 'sub { $ans = $` + "}"; }');
    test_block('if($match =~ m/$self->{openers}/) {
','            $terminated = 1 if $front =~ m/^\s*$/;

            $dlev++;
        } elsif ($match =~ m/$self->{closers}/) {',
        'if($match =~ m/$self->{openers}/) {
            $terminated = 1 if $front =~ m/^\s*$/;

            $dlev++;
        }');
    test_block('sub {', ' foo =~ m#}# } foobar', 'sub { foo =~ m#}# }');
};

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
    return $CF->tag() if ref($CF);
}

sub apply_ask {
    if(ref($CF) eq "File" && $CF->{changed}) {
        print "There are unsaved changes in '$CF->{name}', do you want to save it? [y/n/c] ";
        my $ans = <STDIN>;

        return Result::ok() unless $ans && $ans =~ m/\S+/;

        if(lc($&) eq "y") {
            apply();
            return Result::terminate(0);
        } elsif (lc($&) eq "n") {
            return Result::terminate(0);
        }

        return Result::ok();
    }

    return Result::terminate(0)
        unless $CF;

    apply();
    return Result::ok();
}

sub nextm {
    if($CF) {
        $CF->next();
    }
}

sub Prompt::new {
    my ($class, %params) = @_;

    return bless {%params} => $class;
}

sub Prompt::read_command {
    my ($self) = @_;

    my $prompt = ref($self->{prompt}) eq "CODE"? $self->{prompt}->()
                                               : $self->{prompt} || ">";
    
    my $line = $reader->readline($prompt);

    unless(defined $line) {
        return $self->{on_terminated}->()
            if ref($self->{on_terminated}) eq "CODE";

        return Result::terminate(0);
    }

    return Result::ok() unless $line;

    my $proc = $perl_scanner->scan($line);
    while($proc->drain() > 0) {
        my $more = $reader->readline("...>");
        return Result::ok() unless defined $more;
        
        $line .= "\n$more";
        $proc = $perl_scanner->scan($line);
    }

    return $line;
}

sub Prompt::run {
    my($self) = @_;

    my $ctx //= $_;

    my $line = $self->read_command();
    return $line if ref($line) eq "Result";

    $reader->addhistory($line)
        if $line =~ m/S/;

    local $_ = $ctx;
    my $ans = eval $line;
    if($@) {
        print STDERR $@;
        return Result::failed($@);
    } elsif ($ans) {
        print "\$_ = $ans\n";
        $_ = $ans;
    }

    return Result::ok($ans);
}

for my $rc_path ('.perepl', "$ENV{HOME}/.perepl") {
    if(open(my $rc, $rc_path)) {
        eval(join("", <$rc>));
        close($rc);
    }
}

my $main_prompt = new Prompt(
    prompt => sub {
        if(defined $CF) {
            return "\n\$CF=" . $CF->tag() . "> ";
        } else {
            return "\n> ";
        }
    },
    on_terminated => \&apply_ask
);

while(!defined($main_prompt->run()->{terminate})) {
}

