#!/usr/bin/env perl

use strict;
use warnings;
use Term::ReadLine;

sub UNIVERSAL::DISPLAY {
    my ($self,@visited) = @_;

    return "(...)" if (ref($self) && grep { $self == $_ } @visited);

    return "(undef)" if !defined $self;

    if(defined &{ref($self) . "::DISPLAY"}) {
        return $self->DISPLAY(@visited);
    }

    if(UNIVERSAL::isa($self, "HASH")) {
        my $bless = (ref($self) eq "HASH")?"":ref($self);
        return $bless . "{ " . join(",", map { "$_ => " . UNIVERSAL::DISPLAY($self->{$_}, @visited, $self) }
                                         keys %$self)
                          . "}";
    } elsif (UNIVERSAL::isa($self, "ARRAY")) {
        my $bless = (ref($self) eq "ARRAY")?"":ref($self);
        return $bless . "[ " . join(",", map { UNIVERSAL::DISPLAY($_, @visited, $self) } @$self) . "]";
    }

    return "'$self'";
}

my $test_start;
sub test(&) {
    my ($fun) = @_;

    my ($pack, $fname, $line) = caller(0);

    $test_start = "$fname:$line";

    $fun->();
}

sub list_of(&$) {
    my ($code, $init) = @_;

    my @ans = ($init);
    my $next = $init;
    while($next = $code->(local $_ = $next)) {
        push(@ans, $next);
    }

    return @ans;
}

sub Result::ok {
    my ($val) = @_;

    return bless {val => ($val || 1)} => "Result";
}

sub Result::failed {
    my ($msg) = @_;

    return bless { err => $msg } => "Result";
}

sub Result::failed_if {
    my ($fn) = @_;

    my $ans = $fn->();
    if($ans) {
        return Result::ok();
    } else {
        return Result::failed($ans);
    }
}

sub Result::is_ok {
    my ($self) = @_;

    return $self->{val};
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

sub Result::then {
    my ($self, $then, $else) = @_;

    if(defined $self->{val}) {
        local $_ = $self->{val};
        return $then->();
    }

    if($else) {
        local $_ = $self->{err};
        return $else->();
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

@File::ISA = ("Buffer");

sub File::new {
    my ($class, $filename) = @_;

    my $self = bless { 
        name => $filename,
        change_log => []
    } => $class;

    $self->read();
    return $self;
}

sub File::DISPLAY {
    my ($self) = @_;
    return "File<$self->{name}>";
}

sub File::commited_with {
    my ($self, $commiter) = @_;

    $self->{commiter} = $commiter;
    return $self;
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
        if(!open(my $fh, "$self->{name}")) {
            $self->{content} = "";
        } else {
            $self->{content} = join("", <$fh>);
            $self->{change_log} = [[ 0, length($self->{content}), 0]];

            close($fh);
        }
    };

    return $self->{content};
}

sub Buffer::map_region {    
    my ($self, $base_version, $back_size, $match_size, $front_size) = @_;

    my @change_log = @{$self->{change_log}};
    my @csize = @{$change_log[$base_version]};

    for my $log_item (@change_log[$base_version+1..$#change_log]) {
        my ($bs, $ms, $fs) = @$log_item;
        my ($b, $m, $f) = @csize;
        my $ds = $bs + $ms + $fs - $b - $m - $f;
        @csize = @$log_item;        
        if ($bs <= $back_size) {
            if($ds > 0) {
                $back_size += $ds;
            } else {
                my $db_max = $back_size - $bs;
                if(-$ds > $db_max) {
                    $back_size = $bs;
                    $front_size += $ds + $db_max;
                } else {
                    $back_size += $ds;
                }
            }
        } else {
            $front_size += $ds
        }
    }

    return $back_size, $front_size;
}

test {
    my $buff = bless { 
         change_log => [[0, 200, 0], [0, 20, 200], [100, 20, 100], [200, 20, 20], [100, 0, 100]]
    } => "Buffer";
    assert_eq(display_all($buff->map_region(0, 40, 10, 160)), "( '60', '140' )");
    assert_eq(display_all($buff->map_region(3, 110, 20, 110)), "( '100', '80' )");
    assert_eq(display_all($buff->map_region(0, 200, 50, 0)), "( '200', '0' )");
};

sub Buffer::read {
    my ($self) = @_;

    return $self->{content};
}

sub Buffer::paste {
    my ($self, $sel) = @_;

    my ($bs, $ms, $fs) = map { length($_) }
                             @$sel{qw/back match front/};

    my @mapping = $self->map_region($sel->{base_version}, $bs, $ms, $fs);
    
    $self->modify($sel->read(), @mapping);
}

test {
    my $base = bless { change_log => [[0, 10, 0]], content => "Foobar Baz" } => "Buffer";
    my $deriv = new Selection($base, "bar")->next();
    $deriv->modify(" Bar");

    $base->modify("Qoo", 0, 10);
    assert_eq($base->{content}, "QooFoobar Baz");
    $base->paste($deriv);

    assert_eq($base->{content}, "QooFoo Bar Baz");
};

sub Buffer::append {
    my ($self, $txt) = @_;

    push(@{$self->{change_log}}, [ length($self->{content}), length($txt), 0 ]);
    $self->{content} .= $txt;
    $self->{changed} = 1;

    return $self;
}

sub Buffer::modify {
    my ($self, $val, $after, $leave) = @_;

    if(ref($val) eq "CODE") {
        local $_ = $self->{content};
        $val = $val->();
    }

    if($self->{content} ne $val) {
        if(!defined($after)) {
            $self->{content} = $val;
        } else {
            $self->{content} = substr($self->read(), 0, $after)
                               . $val
                               . substr($self->read(), -$leave);
        }

        push(@{$self->{change_log}}, [ $after//0, length($val), $leave//0 ]);
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

        if(defined $self->{commiter}) {
            local $_ = $self;
            $self->{commit_result} = $self->{commiter}->();
            
            if(!$self->{commit_result}->is_ok()) {
                print STDERR "Commiting changes in '$self->{name}' failed:\n$self->{commit_result}->{err}\n";
                return $self;
            } else {
                return $self->{base};
            }
        }
    }

    return $self->{base};
}

@Variable::ISA = ("Buffer");

sub Variable::new {
    my ($class, $src) = @_;

    return bless {
        ref => $src,
        content => $$src
    } => $class;

}

sub Variable::read {
    my ($self) = @_;

    return $self->{content};
}

sub Variable::tag {
    my ($self) = @_;

    return "VAR";
}

sub Variable::apply {
    my ($self) = @_;

    if($self->{changed}) {
        ${$self->{ref}} = $self->{content};
    }

    return undef;
}

@Selection::ISA = qw/Buffer/;
sub Selection::new {
    my ($class, $base, $pattern, $capture) = @_;

    $capture = [$capture]
        if($capture && ref($capture) ne "ARRAY");

    return bless {
        base => $base,
        pattern => $pattern,
        capture => $capture,
        line => 1,
        sel_lines => 0,

        change_log => [],
        base_version => scalar(@{$base->{change_log}}) - 1,

        back => "",
        match => "",
        front => $base->read()
    } => $class;
}

sub Selection::DISPLAY {
    my ($self) = @_;

    my $root = $self->root();
    my $fname = $root?$root->{name}:"(UNNAMED)";
    return "Selection<$fname\@" . $self->line() .">";
}

sub Selection::line {
    my ($self) = @_;

    my $line = $self->{line};
    while(ref($self->{base}) eq "Selection") {
        $self = $self->{base};
        $line += $self->{line} - 1;
    }

    return $line;
}

sub Selection::root {
    my ($self) = @_;

    my $base = $self->{base};
    while($base && $base->{base}) { $base = $base->{base} }

    return $base;
}

sub Selection::refresh_lines {
    my ($self) = @_;

    my @in_nls = $self->{match} =~ m/\n/g;
    $self->{sel_lines} = @in_nls;
}

sub Selection::next {
    my ($self) = @_;

    if ($self->{front} && $self->{front} =~ m/$self->{pattern}/) {
        my $back = $`;
        $self->{back} .= $self->{match} . $back;
        $self->{match} = $&;
        $self->{front} = $';
        $self->{change_log} = [ [0, length($self->{match}), 0] ];

        if(defined $self->{capture}) {
            for(my $i = 0; $i < @{$self->{capture}}; $i++) {
                $self->{$self->{capture}->[$i]} = eval('$'. ($i+1));
            }
        }

        my @nls = $back =~ m/\n/g;
        $self->{line} += @nls + $self->{sel_lines};
        $self->refresh_lines();

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
        $self->{back} =~ m/([^\n]*)(?!\n)$/s;
        my $backself = $1 // "";

        $self->{front} =~ m/^([^\n]*)/s;
        my $frontself = $1 // "";

        if(ref($self->{base}) eq "File") {
            my @matchlines = split("\n", $self->{match});
            my $i = $self->{line};
            my $tag = join("\n", map { s/\e/[33m^ESC[0m/g; $i++ . ": $_" } @matchlines);
            return "[90m$backself[0m$tag[90m$frontself[0m";
        } else {
            return "[90m$backself[0m$self->{match}[90m$frontself[0m";
        }
    }

    return "end";
}

sub Selection::append {
    my ($self, $txt) = @_;

    $self->{match} .= $txt;
    $self->refresh_lines();
    $self->{changed} = 1;

    return $self;
}

sub Selection::extend {
    my ($self, $pattern) = @_;

    if($self->{front} =~ $pattern) {
        $self->{match} .= $` . $&;
        $self->{front} = $';
        $self->refresh_lines();
    }
}

sub Selection::modify {
    my ($self, $val, $after, $leave) = @_;

    my $newval;
    if(ref($val) eq "CODE") {
        local $_ = $self->{match};
        $newval = $val->();
    } else {
        $newval = $val;
    }

    if(!defined($after)) {
        $self->{match} = $newval;
    } else {
        $self->{match} = substr($self->{match}, 0, $after)
                         . $newval
                         . substr($self->{match}, -$leave);
    }

    $self->refresh_lines();
    $self->{changed} = 1;

    return $self;
}

sub Selection::as_list {
    my ($self, $mapper) = @_;

    my @ans;
    while($self->next(), my $x = $self->{match}) {
        if(defined $mapper) {
            local $_ = $x;
            $x = $mapper->();
        }
        push(@ans, $x);
    }

    return @ans;
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
        } elsif ($match =~ m/$self->{line_limiters}/ && $front =~ m/^\s*(#[^\n]*)?$/) {
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
    test_indent("sub foo {\n my \$bar = '\\}'; #foobar\nassert_eq(\$foo,\n'\\}'\n);\n}",
                "sub foo {\n    my \$bar = '\\}'; #foobar\n    assert_eq(\$foo,\n        '\\}'\n    );\n}");
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

    $self->{base}->paste($self)
        if($self->{changed});

    delete $self->{changed};

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

sub Scanning::DISPLAY {
    my ($self) = @_;
    return "Scanning<"
        . substr($self->{back}, -10)
        . "|" . substr($self->{front}, 0, 10)
        . "\@$self->{level}>";
}

sub noop { return 1; }

sub returner {
    my ($val) = @_;

    return sub { return $val; };
}

sub expect_delimiter {
    my ($delimiter) = @_;

    return sub {
        my($ctx, $back, $match, $front) = @_;

        my $base_scanner = $ctx->{scanner};
        $ctx->{level}++;
        $ctx->{scanner} = new Scanner('\\\\.' => \&noop,
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
    ";" => returner(";"),
    '\(' => sub {
        my($ctx) = @_;
        $ctx->{level}++;
        return $ctx->{level};
    },
    '\)' => sub {
        my($ctx) = @_;
        $ctx->{level}--;

        return $ctx->{level};
    },
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

test {
    my $proc = $perl_scanner->scan(qq[extend("'\\"")]);
    assert_eq($proc->drain(), 0);
};

sub block {
    my ($sel) = @_;

    my $proc = $perl_scanner->scan($sel->{front});

    while(1) {
        my $ans = $proc->step();
        return unless defined $ans;

        if($proc->{level} <= 0) {
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
my @buffers;

sub switch_buff {
    my($buff) = @_;

    if(defined $CF) {
        push(@buffers, $CF);
    }

    $CF = $buff;
}

sub first_that {
    my ($filter, @array) = @_;

    for(my $i = 0; $i < @array; $i++) {
        local $_ = $array[$i];
        return $i if($filter->());
    }

    return undef;
}

sub change_buff {
    my ($id) = @_;

    if($id !~ m/^\d+$/) {
        $id = first_that(sub { $_ eq $id || m/$id/ }, @buffers);
    }

    if($buffers[$id]) {
        push(@buffers, $CF);
        $CF = $buffers[$id];
        splice(@buffers, $id, 1);
    }

    return $CF;
}

sub stash {
    if($CF) {
        push(@buffers, $CF);
        $CF = $CF->{base};
    }

    return $CF;
}

sub run {
    my ($val) = @_;

    $val //= $CF->{match};
    if($val) {
        my $ans = eval $val;
        warn $@ if $@;
        return $ans;
    }
}

sub edit {
    my ($name) = @_;

    switch_buff(File->new($name));
}

my %funsrc;

sub funed {
    my ($name) = @_;

    if(defined($funsrc{$name})) {
        switch_buff($funsrc{$name});
    }
}

sub sel {
    my ($selector) = @_;

    die "You must open a file first (use edit)"
        unless defined $CF;

    my $sel = new Selection($CF, $selector)->next();
    $CF = $sel if $sel->{match};

    return $CF;
}

sub lines {
    my ($filter) = @_;

    sel("[^\n]+");
    
    $CF->filter(sub { $_[0]->{match} =~ m/$filter/ })
        if($filter);
}

sub line {
    my ($no) = @_;

    if(defined($CF)) {
        $no -= $CF->{line} if ref($CF) eq "Selection";
        sel("[^\n]*\n");
        while($no-- > 0) { $CF->next() }
    }
}

sub funs {
    my ($filter) = @_;

    my $sel = new Selection($CF, 'sub\s+([\w:]+)(\s*\(.+?\))?', "name")->next();

    $sel->map(\&block);
    $sel->filter(sub { $_[0]->{match} =~ m/sub\s*$filter/ })
        if defined $filter;

    if($sel->{match}) {
        $CF = $sel;
    }
    return $CF
}
sub vars {
    my ($filter) = @_;

    my $sel = new Selection($CF, 
        '(my|our|local)\s+([\$%@]\w+)\s*=', 
        ['scope', 'name']
    )->next();

    $sel->filter(sub { $_[0]->{name} =~ m/$filter/ });

    $sel->map(sub {
            my ($sel) = @_;

            my $proc = $perl_scanner->scan($sel->{front});
            while(1) {
                my $ans = $proc->step();
                if(!defined $ans || ( $ans eq ";" && $proc->{level} == 0)) {
                    last;
                }

            }

            $sel->{match} .= $proc->{back};
            $sel->{front} = $proc->{front};

            return $sel;
    });

    $CF = $sel;
    return $CF;
}


sub append {
    if(ref($CF) eq "Selection") {
        my $txt = "";
        while(defined(my $line = $reader->readline('"'))) {
            $txt .= "$line\n";
        }

        $CF->append($txt);
    }
}

sub extend {
    my ($pattern) = @_;

    if(ref($CF) eq "Selection") {
        $CF->extend($pattern);
    }
}

my $APPEND_AUTOINDENT = 1;

sub apply {
    my ($autoindent) = @_;

    $autoindent //= $APPEND_AUTOINDENT;

    return 0 unless defined $CF;
    
    $CF = $CF->apply();

    if(!defined $CF && @buffers) {
        $CF = pop(@buffers);
    }

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
            return Result::ok();
        } elsif (lc($&) eq "n") {
            cancel();
            return Result::ok();
        } else {
            return Result::ok();
        }
    }

    return Result::terminate(0)
        unless $CF;

    apply();
    return Result::ok();
}

sub commit {
    if(defined($CF)) {
        eval $CF->read();
    }
}

sub cancel {
    if(defined($CF)) {
        $CF = $CF->{base};
        if(!$CF && @buffers) {
            $CF = pop(@buffers);
        }
    }
}

sub nextm {
    if($CF) {
        $CF->next();
    }
    cancel() if !$CF->{match};
}

sub Prompt::new {
    my ($class, %params) = @_;

    my $obj = bless {%params} => $class;
    $obj->{default} //= sub { return Result::ok() };

    return $obj;
}

sub eval_prompt_macros {
    my ($line) = @_;

    if($line =~ m#^\s*(\w*)/((\\.|[^/])*)(\s*$|/(.*))#) {
        my ($cmd, $param, $rest) = ($1, $2, $4);
        if(!$cmd || $cmd eq "m") {
            return "sel('$param');";
        }
        elsif($cmd eq "s") {
            $rest =~ m#/(.*)(/|$)#;
            $rest = $1 || "";
            return "\$CF->modify(sub{s/$param/$rest/g; \$_});";
        } elsif ($cmd eq "a") {
            return "\$CF->append('$param');";
        } elsif ($cmd eq "i") {
            return "\$CF->modify(sub{\"$param\" . \$_});";
        } elsif ($cmd eq "r") {
            return "\$CF->modify(\"$param\");";
        } elsif ($cmd eq "d") {
            if($CF->{match} !~ m/\n$/) {
                $CF->extend("\n");
            }

            $CF->modify("");
            $CF->next();
            return 1;
        } elsif ($cmd eq "e") {
            eval { $CF->extend($param||"\n") };
            print STDERR "Syntax error: $@\n"
                if $@;
            return 1;
        }
    } 
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

    return $self->{default}->($self) unless $line;

    my $ret = eval_prompt_macros($line);
    return $ret if $ret;

    my $proc = $perl_scanner->scan($line);
    while($proc->drain() > 0) {
        my $more = $reader->readline("...>");
        return Result::ok() unless defined $more;
        
        $line .= "\n$more";
        $proc = $perl_scanner->scan($line);
    }

    return $line;
}

sub display_all {
    if(@_ > 1) {
        return "( " . join(", ", map { UNIVERSAL::DISPLAY($_) } @_) ." )";
    } else {
        return UNIVERSAL::DISPLAY($_[0]);
    }
}

sub Prompt::run {
    my($self) = @_;

    my $ctx //= $_;

    my $line = $self->read_command();
    return $line if ref($line) eq "Result";

    $reader->addhistory($line)
        if $line =~ m/S/;

    local $_ = $ctx;
    my @ans = eval "no strict 'vars'; $line";
    if(@ans == 0 && $@) {
        print STDERR $@;
        return Result::failed($@);
    } elsif (@ans) {
        print "\$_ = " . display_all(@ans) . "\n";
    }

    if($line =~ m/^\s*sub\s+(\w+)/) {
        $funsrc{$1} = new Variable(\$line);
    }

    $_ = $ans[0];

    return Result::ok($ans[0]);
}

my $main_prompt = new Prompt(
    prompt => sub {
        if(defined $CF) {
            if(ref($CF) eq "Selection") {
                print "\n" . $CF->tag() . "\n";
                my $cur = $CF;
                $cur = $cur->{base}
                    while($cur && ref($cur) eq "Selection");
                
                return '$CF=' . $cur->tag() . "> ";
            } else {
                return "\n\$CF=" . $CF->tag() . "> ";
            }
        } else {
            return "\n> ";
        }
    },
    on_terminated => \&apply_ask,
    default => sub {
        if(defined $CF) { eval "nextm()"; }
        return Result::ok();
    }
);

sub eval_file {
    my ($name) = @_;

    my $in;
    return Result::failed_if(sub { open($in, $name) })
                 ->then(sub {
                        my $code = "no strict 'vars'; " . join("\n", <$in>);
                        my $ret = eval($code);
    
                        if(defined $ret) {
                            print STDERR "Warnings from $name:\n$@\n"
                                if $@;
    
                            return Result::ok($name);
                        } else {
                            return Result::failed($@);
                        }
                    });
}

my $loader_prompt = new Prompt(%$main_prompt,
                    on_terminated => sub {
                        if(!defined $CF->{base}) {
                            my $new_cf = $CF->apply();
                            if($new_cf//0 == $CF) {
                                print STDERR "Do you want to continue? [y/n] ";
                                my $ans = <STDIN>;

                                if($ans =~ m/^\s*n\s*$/i) {
                                    return Result::terminate(1);
                                } else {
                                    return Result::ok();
                                }
                            } else {
                                $CF = $new_cf;
                                return Result::terminate(0);
                            }
                        } else {
                            $CF = $CF->apply();
                            return Result::ok();
                        }
                    });
                    
sub load_file {
    my ($name) = @_;

    eval_file($name)
        ->then(sub { },
               sub { 
                   print STDERR "Fix in '$name':\n$_";
                   edit($name);
                   $CF->commited_with(sub {
                        my $ret = eval($_->{content});
                        if(defined $ret) {
                            return Result::ok($ret);
                        } else {
                            return Result::failed($@);
                        }
                   });

                   my $ans;
                   while(!defined($ans = $loader_prompt->run()->{terminate})) {}

                   if($ans == 1) {
                       die "File not fixed, aborting";
                   }
                });
}

for my $rc_path ('.perepl', "$ENV{HOME}/.perepl") {
    if(-e $rc_path) {
        load_file($rc_path);
    }
}

while(!defined($main_prompt->run()->{terminate})) {
}

