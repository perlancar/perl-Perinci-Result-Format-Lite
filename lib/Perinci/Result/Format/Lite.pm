package Perinci::Result::Format::Lite;

# DATE
# VERSION

use 5.010001;
#IFUNBUILT
use strict;
use warnings;
#END IFUNBUILT

use List::Util qw(first);

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(format);

# copy-pasted from List::MoreUtils::PP
sub firstidx (&@) {
    my $f = shift;
    foreach my $i ( 0 .. $#_ )
        {
            local *_ = \$_[$i];
            return $i if $f->();
        }
    return -1;
}

sub _json {
    state $json = do {
        if    (eval { require JSON::XS; 1 })   { JSON::XS->new->canonical(1)->allow_nonref }
        elsif (eval { require JSON::Tiny::Subclassable; 1 }) { JSON::Tiny::Subclassable->new }
        elsif (eval { require JSON::PP; 1 })   { JSON::PP->new->canonical(1)->allow_nonref }
        else { die "Can't find any JSON module" }
    };
    $json;
};

sub __cleanse {
    state $cleanser = do {
        eval { require Data::Clean::JSON; 1 };
        if ($@) {
            undef;
        } else {
            Data::Clean::JSON->get_cleanser;
        }
    };
    if ($cleanser) {
        $cleanser->clean_in_place($_[0]);
    } else {
        $_[0];
    }
}

sub __gen_table {
    my ($data, $header_row, $resmeta, $format) = @_;

    $resmeta //= {};

    my @columns;
    if ($header_row) {
        @columns = @{$data->[0]};
    } else {
        @columns = map {"col$_"} 0..@{$data->[0]}-1;
    }

    my $column_orders; # e.g. [col2, col1, col3, ...]
  SET_COLUMN_ORDERS: {

        # find column orders from 'table_column_orders' in result metadata (or
        # from env)
        my $tcos;
        if ($ENV{FORMAT_PRETTY_TABLE_COLUMN_ORDERS}) {
            $tcos = _json->encode($ENV{FORMAT_PRETTY_TABLE_COLUMN_ORDERS});
        } elsif (my $rfos = ($resmeta->{'cmdline.format_options'} //
                                 $resmeta->{format_options})) {
            my $rfo = $rfos->{'text-pretty'} // $rfos->{text} // $rfos->{any};
            if ($rfo) {
                $tcos = $rfo->{table_column_orders};
            }
        }
        if ($tcos) {
            # find an entry in tcos that @columns contains all the columns of
          COLS:
            for my $cols (@$tcos) {
                for my $col (@$cols) {
                    next COLS unless first {$_ eq $col} @columns;
                }
                $column_orders = $cols;
                last SET_COLUMN_ORDERS;
            }
        }

        # find column orders from table spec
        $column_orders = $resmeta->{'table.fields'};
    }

    # reorder each row according to requested column order
    my @map;
    if ($column_orders) {
        # 0->2, 1->0, ... (map column position from unordered to ordered)
        my @map0 = sort {
            my $idx_a = firstidx(sub {$_ eq $a->[1]},
                                 @$column_orders) // 9999;
            my $idx_b = firstidx(sub {$_ eq $b->[1]},
                                 @$column_orders) // 9999;
            $idx_a <=> $idx_b || $a->[1] cmp $b->[1];
        } map {[$_, $columns[$_]]} 0..@columns-1;
        #use DD; dd \@map0;
        for (0..@map0-1) {
            $map[$_] = $map0[$_][0];
        }
        #use DD; dd \@map;
        my $newdata = [];
        for my $row (@$data) {
            my @newrow;
            for (0..@map-1) { $newrow[$_] = $row->[$map[$_]] }
            push @$newdata, \@newrow;
        }
        $data = $newdata;
    } else {
        @map = 0..$#columns;
    }

    if ($header_row && (my $tfu = $resmeta->{'table.field_units'}) && @$data) {
        for (0..$#map) {
            if (defined $tfu->[$_]) {
                $data->[0][$_] .= " ($tfu->[$_])";
            }
        }
    }

    if ($format eq 'text-pretty') {
        require Text::Table::Tiny;
        Text::Table::Tiny::table(rows=>$data, header_row=>$header_row) . "\n";
    } elsif ($format eq 'csv') {
        no warnings 'uninitialized';
        join(
            "",
            map {
                my $row = $_;
                join(
                    ",",
                    map {
                        my $cell = $_;
                        $cell =~ s/(["\\])/\\$1/g;
                        qq("$cell");
                    } @$row)."\n";
            } @$data
        );
    } else {
        no warnings 'uninitialized';
        shift @$data if $header_row;
        join("", map {join("\t", @$_)."\n"} @$data);
    }
}

sub format {
    my ($res, $format, $is_naked, $cleanse) = @_;

    if ($format =~ /\A(text|text-simple|text-pretty|csv)\z/) {
        $format = $format eq 'text' ?
            ((-t STDOUT) ? 'text-pretty' : 'text-simple') : $format;
        no warnings 'uninitialized';
        if ($res->[0] !~ /^(2|304)/) {
            my $fres = "ERROR $res->[0]: $res->[1]";
            if (my $prev = $res->[3]{prev}) {
                $fres .= " ($prev->[0]: $prev->[1])";
            }
            return "$fres\n";
        } elsif ($res->[3] && $res->[3]{"x.hint.result_binary"}) {
            return $res->[2];
        } else {
            require Data::Check::Structure;
            my $data = $res->[2];
            my $max = 5;
            if (!ref($data)) {
                $data //= "";
                $data .= "\n" unless !length($data) || $data =~ /\n\z/;
                return $data;
            } elsif (ref($data) eq 'ARRAY' && !@$data) {
                return "";
            } elsif (Data::Check::Structure::is_aos($data, {max=>$max})) {
                return join("", map {"$_\n"} @$data);
            } elsif (Data::Check::Structure::is_aoaos($data, {max=>$max})) {
                return __gen_table($data, 0, $res->[3], $format);
            } elsif (Data::Check::Structure::is_hos($data, {max=>$max})) {
                $data = [map {[$_, $data->{$_}]} sort keys %$data];
                unshift @$data, ["key", "value"];
                return __gen_table($data, 1, $res->[3], $format);
            } elsif (Data::Check::Structure::is_aohos($data, {max=>$max})) {
                # collect all mentioned fields
                my @fieldnames;
                if ($res->[3] && $res->[3]{'table.fields'} &&
                        $res->[3]{'table.hide_unknown_fields'}) {
                    @fieldnames = @{ $res->[3]{'table.fields'} };
                } else {
                    my %fieldnames;
                    for my $row (@$data) {
                        $fieldnames{$_}++ for keys %$row;
                    }
                    @fieldnames = sort keys %fieldnames;
                }
                my $newdata = [];
                for my $row (@$data) {
                    push @$newdata, [map {$row->{$_}} @fieldnames];
                }
                unshift @$newdata, \@fieldnames;
                return __gen_table($newdata, 1, $res->[3], $format);
            } else {
                $format = 'json-pretty';
            }
        }
    }

    my $tff = $res->[3]{'table.fields'};
    $res = $res->[2] if $is_naked;

    unless ($format =~ /\Ajson(-pretty)?\z/) {
        warn "Unknown format '$format', fallback to json-pretty";
        $format = 'json-pretty';
    }
    __cleanse($res) if ($cleanse//1);
    if ($format =~ /json/) {
        if ($tff && _json->can("sort_by") &&
                eval { require Sort::ByExample; 1}) {
            my $cmp = Sort::ByExample->cmp($tff);
            _json->sort_by(sub { $cmp->($JSON::PP::a, $JSON::PP::b) });
        }

        if ($format eq 'json') {
            return _json->encode($res) . "\n";
        } else {
            _json->pretty(1);
            return _json->encode($res);
        }
    }
}

1;
# ABSTRACT: Format enveloped result

=for Pod::Coverage ^(firstidx)$

=head1 SYNOPSIS


=head1 DESCRIPTION


=head1 FUNCTIONS

=head2 format($res, $format[ , $is_naked=0, $cleanse=1 ]) => str


=head1 ENVIRONMENT

=head2 FORMAT_PRETTY_TABLE_COLUMN_ORDERS => array (json)

Set the default of C<table_column_orders> in C<format_options> in result
metadata, similar to what's implemented in L<Perinci::Result::Format> and
L<Data::Format::Pretty::Console>.


=head1 SEE ALSO

L<Perinci::Result::Format>, a more heavyweight version of this module.

L<Perinci::CmdLine::Lite> uses this module to format enveloped result.

=cut
