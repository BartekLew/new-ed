package Perepl::Java;

use strict;
use warnings;
use Exporter;

our @EXPORT = qw/classes methods rglob classes/;
our @ISA = qw/Exporter/;

sub rglob {
    my ($exp) = @_;

    my @path = split("/", $exp);
    my @ans = (shift @path);

    for my $node (@path) {
        if($node eq "**") {
           my $changed;

           my @new = @ans;
           do {
               @new = grep { -d } map { glob("$_/*") } @new;
               @ans = (@ans, @new);
           } while(@new);
        } else {
            @ans = map { glob("$_/$node") } @ans;
        }
    }

    return @ans;
}

sub subhash {
    my ($ref, @keys) = @_;

    return map { $_ => $ref->{$_} } @keys;
}

sub UNIVERSAL::clone {
    my ($self, %params) = @_;

    my $bless = $params{bless} // ref($self);

    if(ref($self) && $params{keep_ref} && grep { $_ == $self } @{$params{keep_ref}}) {
        return $self;
    }

    if(UNIVERSAL::isa($self, "HASH")) {
        my $obj = { map { $_ => UNIVERSAL::clone($self->{$_}, keep_ref=>$params{keep_ref}) }
                        keys %$self };
        if($bless ne "HASH") {
            $obj = bless $obj => $bless;
        }
        return $obj;
    } elsif (UNIVERSAL::isa($self, "ARRAY")) {
        my $obj = [ map { UNIVERSAL::clone($_, keep_ref=>$params{keep_ref}) } @$self ];

        if($bless ne "ARRAY") {
            $obj = bless $obj => $bless;
        }
        return $obj;
    } elsif (ref($self)) {
        return $self;
    } else {
        return $self;
    }
}

sub JavaMethod::new {
    my ($class, $sel) = @_;

    return bless $sel->clone(bless => $class, keep_ref => [ $sel->{base} ]) => $class;
}

sub JavaMethod::DISPLAY {
    my ($self) = @_;

    my $class = (defined $self->{base} && $self->{base}->{name})?"$self->{base}->{name}::":"";
    my @mods = map { s/\s+$//; $_ } 
                   grep { defined $_ && $_ ne "" } @$self{qw/range staticness type/};
    my $name = $self->{name};
    my $params = $self->{params};

    return "JavaMethod<" . join(" ", @mods) . (@mods>0? " ": "") . "$class$name($params)>";
}

sub JavaClass::new {
    my ($class, $sel) = @_;

    my $self = $sel->clone(bless => $class, keep_ref => [ $sel->{base} ]);
    
    my $method_sel = new Selection($sel,
                            '\s+(public\s+|private\s+|package\s+)?(static\s+)?(\w+)\s+(\w+)\s*\(([^)]*)\)',
                            [qw/range staticness type name params/]
                     )
                     ->map(\&main::block);
    $self->{methods} = [ $method_sel->as_list(sub {new JavaMethod($method_sel)}) ];

    return $self;
} 

sub JavaClass::DISPLAY {
    my ($self) = @_;

    return "JavaClass<$self->{name}>";
}

@JavaSource::ISA = qw/File/;
sub JavaSource::new {
    my ($class, $filename) = @_;

    my $self = File::new($class, $filename);
    $self->read() =~ m/^\s*package\s+([\w.]+);/;
    $self->{package} = $1 // "";
    
    my $import_sel = new Selection($self, '\simport\s+([^;]+);', 'classname');
    $self->{imports} = [ $import_sel->as_list(sub { $import_sel->{classname} }) ];

    my $class_sel = new Selection($self, '\bclass\s+(\w+)\s*([^{]+)', [qw/name params/])
                            ->map(\&main::block);
    my @classes;
    while ($class_sel->next() && $class_sel->{match}) {
        push(@classes, new JavaClass($class_sel));
    }
    $self->{classes} = [ @classes ];

    $self->compile();

    return $self;
}

sub JavaSource::DISPLAY {
    my ($self) = @_;

    return "JavaSource<$self->{name}>";
}

sub JavaSource::compile {
    my ($self) = @_;

    open(my $out, "javac $self->{name} 2>&1 |") 
        or die "Can't run javac on '$self->{name}': $!";

    $self->{compiler_output} = join("\n", <$out>);
    $self->{status} = close($out);
}

sub JavaProject::new {
    my ($class, $glob) = @_;

    return bless { sources =>  [ map { new JavaSource($_) }
                                     rglob($glob // "src/**/*.java") ] }
                 => $class;
}

my $project = new JavaProject();
sub classes {
    return map { map { $_->{name} } @{$_->{classes}} } @{$project->{sources}}
}

1;
