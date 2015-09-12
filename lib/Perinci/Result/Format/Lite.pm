package Perinci::Result::Format::Lite;

use 5.010001;
#IFUNBUILT
use strict;
use warnings;
#END IFUNBUILT

use List::Util qw(first);

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(format);

sub __json {
    state $json = do {
        require JSON;
        JSON->new->canonical(1)->allow_nonref;
    };
    $json;
}

sub __cleanser {
    state $cleanser = do {
        require Data::Clean::JSON;
        Data::Clean::JSON->get_cleanser;
    };
    $cleanser;
}

sub __gen_table {
    my ($data, $header_row, $resmeta, $is_pretty) = @_;

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
            $tcos = __json->decode($ENV{FORMAT_PRETTY_TABLE_COLUMN_ORDERS});
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
    if ($column_orders) {
        require List::MoreUtils;

        # 0->2, 1->0, ... (map column position from unordered to ordered)
        my @map0 = sort {
            my $idx_a = List::MoreUtils::firstidx(sub {$_ eq $a->[1]},
                                                  @$column_orders) // 9999;
            my $idx_b = List::MoreUtils::firstidx(sub {$_ eq $b->[1]},
                                                  @$column_orders) // 9999;
            $idx_a <=> $idx_b || $a->[1] cmp $b->[1];
        } map {[$_, $columns[$_]]} 0..@columns-1;
        #use DD; dd \@map0;
        my @map;
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
    }

    if ($is_pretty) {
        require Text::Table::Tiny;
        Text::Table::Tiny::table(rows=>$data, header_row=>$header_row) . "\n";
    } else {
        no warnings 'uninitialized';
        shift @$data if $header_row;
        join("", map {join("\t", @$_)."\n"} @$data);
    }
}

sub format {
    my ($res, $format, $is_naked) = @_;

    if ($format =~ /\Atext(-simple|-pretty)?\z/) {
        my $is_pretty = $format eq 'text-pretty' ? 1 :
            $format eq 'text-simple' ? 0 : (-t STDOUT);
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
                return __gen_table($data, 0, $res->[3], $is_pretty);
            } elsif (Data::Check::Structure::is_hos($data, {max=>$max})) {
                $data = [map {[$_, $data->{$_}]} sort keys %$data];
                unshift @$data, ["key", "value"];
                return __gen_table($data, 1, $res->[3], $is_pretty);
            } elsif (Data::Check::Structure::is_aohos($data, {max=>$max})) {
                # collect all mentioned fields
                my %fieldnames;
                for my $row (@$data) {
                    $fieldnames{$_}++ for keys %$row;
                }
                my @fieldnames = sort keys %fieldnames;
                my $newdata = [];
                for my $row (@$data) {
                    push @$newdata, [map {$row->{$_}} @fieldnames];
                }
                unshift @$newdata, \@fieldnames;
                return __gen_table($newdata, 1, $res->[3], $is_pretty);
            } else {
                $format = 'json-pretty';
            }
        }
    }

    $res = $res->[2] if $is_naked;

    warn "Unknown format '$format', fallback to json-pretty"
        unless $format =~ /\Ajson(-pretty)?\z/;
    __cleanser->clean_in_place($res);
    if ($format eq 'json') {
        return __json->encode($res) . "\n";
    } else {
        return __json->canonical(1)->pretty->encode($res);
    }
}

1;
# ABSTRACT: Format enveloped result

=for Pod::Coverage ^()$

=head1 SYNOPSIS


=head1 DESCRIPTION


=head1 FUNCTIONS

=head2 format($res, $format[ , $is_naked ]) => str


=head1 ENVIRONMENT

=head2 FORMAT_PRETTY_TABLE_COLUMN_ORDERS => array (json)

Set the default of C<table_column_orders> in C<format_options> in result
metadata, similar to what's implemented in L<Perinci::Result::Format> and
L<Data::Format::Pretty::Console>.


=head1 SEE ALSO

L<Perinci::Result::Format>, a more heavyweight version of this module.

L<Perinci::CmdLine::Lite> uses this module to format enveloped result.

=cut
