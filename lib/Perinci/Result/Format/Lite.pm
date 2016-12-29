package Perinci::Result::Format::Lite;

# DATE
# VERSION

use 5.010001;
#IFUNBUILT
use strict;
use warnings;
#END IFUNBUILT

use List::Util qw(first max);

use Exporter qw(import);
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
        if    (eval { require Cpanel::JSON::XS; 1 })   { Cpanel::JSON::XS->new->canonical(1)->convert_blessed->allow_nonref }
        elsif (eval { require JSON::Tiny::Subclassable; 1 }) { JSON::Tiny::Subclassable->new }
        elsif (eval { require JSON::PP; 1 })   { JSON::PP->new->canonical(1)->convert_blessed->allow_nonref }
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
    if ($column_orders) {
        # 0->2, 1->0, ... (map column position from unordered to ordered)
        my @map0 = sort {
            my $idx_a = firstidx(sub {$_ eq $a->[1]},
                                 @$column_orders) // 9999;
            my $idx_b = firstidx(sub {$_ eq $b->[1]},
                                 @$column_orders) // 9999;
            $idx_a <=> $idx_b || $a->[1] cmp $b->[1];
        } map {[$_, $columns[$_]]} 0..$#columns;
        #use DD; dd \@map0;
        my @map;
        for (0..$#map0) {
            $map[$_] = $map0[$_][0];
        }
        #use DD; dd \@map;
        my $newdata = [];
        for my $row (@$data) {
            my @newrow;
            for (0..$#map) { $newrow[$_] = $row->[$map[$_]] }
            push @$newdata, \@newrow;
        }
        $data = $newdata;
        my @newcolumns;
        for (@map) { push @newcolumns, $columns[$_] }
        @columns = @newcolumns;
    }

    my @field_idxs; # map column to index in table.fields
    {
        my $tff = $resmeta->{'table.fields'} or last;
        for my $i (0..$#columns) {
            $field_idxs[$i] = firstidx { $_ eq $columns[$i] } @$tff;
        }
    }

    # add field units as label suffix to header (" (UNIT)")
    {
        last unless $header_row && @$data;
        my $tff = $resmeta->{'table.fields'} or last;
        my $tfu = $resmeta->{'table.field_units'} or last;
        for my $i (0..$#columns) {
            my $field_idx = $field_idxs[$i];
            next unless $field_idx >= 0;
            next unless defined $tfu->[$field_idx];
            $data->[0][$i] .= " ($tfu->[$field_idx])";
        }
    }

    # format cells
    {
        my $tff   = $resmeta->{'table.fields'} or last;
        my $tffmt = $resmeta->{'table.field_formats'} or last;

        # load required modules
        for my $ffmt (@$tffmt) {
            next unless $ffmt;
            if ($ffmt eq 'iso8601') {
                #require Time::Local;
            }
        }

        for my $i (0..$#{$data}) {
            my $row = $data->[$i];
            for my $j (0..$#columns) {
                next unless defined $row->[$j];
                my $field_idx = $field_idxs[$j];
                next unless $field_idx >= 0;
                my $ffmt = $tffmt->[$field_idx];
                next unless $ffmt;
                if ($ffmt eq 'iso8601_datetime' || $ffmt eq 'iso8601_date') {
                    if ($row->[$j] =~ /\A[0-9]+\z/) {
                        my @t = gmtime($row->[$j]);
                        if ($ffmt eq 'iso8601_datetime') {
                            $row->[$j] = sprintf(
                                "%04d-%02d-%02dT%02d:%02d:%02dZ",
                                $t[5]+1900, $t[4]+1, $t[3], $t[2], $t[1], $t[0]);
                        } else {
                            $row->[$j] = sprintf(
                                "%04d-%02d-%02d",
                                $t[5]+1900, $t[4]+1, $t[3]);
                        }
                    }
                } elsif ($ffmt eq 'boolstr') {
                    $row->[$j] = $row->[$j] ? "yes" : "no";
                } elsif ($ffmt eq 'sci2dec') {
                    if ($row->[$j] =~ /\A(?:[+-]?)(?:\d+\.|\d*\.(\d+))[eE]([+-]?\d+)\z/) {
                        my $n = length($1 || "") - $2; $n = 0 if $n < 0;
                        $row->[$j] = sprintf("%.${n}f", $row->[$j]);
                    }
                }
            }
        }
    }

    if ($format eq 'text-pretty') {
        # align columns
        {
            no warnings 'uninitialized';
            my $tfa = $resmeta->{'table.field_aligns'} or last;
            last unless @$data;

            for my $colidx (0..$#columns) {
                my $field_idx = $field_idxs[$colidx];
                next unless $field_idx >= 0;
                my $align = $tfa->[$field_idx];
                next unless $align;

                # determine max widths
                my $maxw;
                my ($maxw_bd, $maxw_d, $maxw_ad); # before digit, digit, after d
                if ($align eq 'number') {
                    my (@w_bd, @w_d, @w_ad);
                    for my $i (0..$#{$data}) {
                        my $row = $data->[$i];
                        if (@$row > $colidx) {
                            my $cell = $row->[$colidx];
                            if ($header_row && $i == 0) {
                                my $w = length($cell);
                                push @w_bd, 0;
                                push @w_bd, 0;
                                push @w_ad, 0;
                            } elsif ($cell =~ /\A([+-]?\d+)(\.?)(\d*)\z/) {
                                # decimal notation number
                                push @w_bd, length($1);
                                push @w_d , length($2);
                                push @w_ad, length($3);
                            } elsif ($cell =~ /\A([+-]?\d+\.?\d*)([eE])([+-]?\d+)\z/) {
                                # scientific notation number
                                push @w_bd, length($1);
                                push @w_d , length($2);
                                push @w_ad, length($3);
                            } else {
                                # not a number
                                push @w_bd, length($cell);
                                push @w_bd, 0;
                                push @w_ad, 0;
                            }
                        } else {
                            push @w_bd, 0;
                            push @w_d , 0;
                            push @w_ad, 0;
                        }
                    }
                    $maxw_bd = max(@w_bd);
                    $maxw_d  = max(@w_d);
                    $maxw_ad = max(@w_ad);
                    if ($header_row) {
                        my $w = length($data->[0][$colidx]);
                        if ($maxw_d == 0 && $maxw_ad == 0) {
                            $maxw_bd = $w;
                        }
                    }
                }

                $maxw = max(map {
                    @$_ > $colidx ? length($_->[$colidx]) : 0
                } @$data);

                # do the alignment
                for my $i (0..$#{$data}) {
                    my $row = $data->[$i];
                    for my $i (0..$#{$data}) {
                        my $row = $data->[$i];
                        next unless @$row > $colidx;
                        my $cell = $row->[$colidx];
                        next unless defined($cell);
                        if ($align eq 'number') {
                            my ($bd, $d, $ad);
                            if ($header_row && $i == 0) {
                            } elsif (($bd, $d, $ad) = $cell =~ /\A([+-]?\d+)(\.?)(\d*)\z/) {
                                $cell = join(
                                    '',
                                    (' ' x ($maxw_bd - length($bd))), $bd,
                                    $d , (' ' x ($maxw_d  - length($d ))),
                                    $ad, (' ' x ($maxw_ad - length($ad))),
                                );
                            } elsif (($bd, $d, $ad) = $cell =~ /\A([+-]?\d+\.?\d*)([eE])([+-]?\d+)\z/) {
                                $cell = join(
                                    '',
                                    (' ' x ($maxw_bd - length($bd))), $bd,
                                    $d , (' ' x ($maxw_d  - length($d ))),
                                    $ad, (' ' x ($maxw_ad - length($ad))),
                                );
                            }
                            my $w = length($cell);
                            $cell = (' ' x ($maxw - $w)) . $cell
                                if $maxw > $w;
                        } elsif ($align eq 'right') {
                            $cell = (' ' x ($maxw - length($cell))) . $cell;
                        } elsif ($align eq 'middle' || $align eq 'center') {
                            my $w = length($cell);
                            my $n = int(($maxw-$w)/2);
                            $cell = (' ' x $n) . $cell . (' ' x ($maxw-$w-$n));
                        } else {
                            # assumed left
                            $cell .= (' ' x ($maxw - length($cell)));

                        }
                        $row->[$colidx] = $cell;
                    }
                }
            } # for $colidx
        } # END align columns

        my $fres;
        if (my $backend = $ENV{FORMAT_PRETTY_TABLE_BACKEND}) {
            require Text::Table::Any;
            $fres = Text::Table::Any::table(rows=>$data, header_row=>$header_row, backend=>$backend);
        } else {
            require Text::Table::Tiny;
            $fres = Text::Table::Tiny::table(rows=>$data, header_row=>$header_row);
        }
        $fres .= "\n" unless $fres =~ /\R\z/ || !length($fres);
        $fres;
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
    } elsif ($format eq 'html') {
        no warnings 'uninitialized';
        require HTML::Entities;

        my $tfa = $resmeta->{'table.field_aligns'};

        my @res;
        push @res, "<table".($resmeta->{'table.html_class'} ?
                                 " class=\"".HTML::Entities::encode_entities(
                                     $resmeta->{'table.html_class'})."\"" : "").
                                         ">\n";
        for my $i (0..$#{$data}) {
            my $data_elem = $i == 0 ? "th" : "td";
            push @res, "<thead>\n" if $i == 0;
            push @res, "<tbody>\n" if $i == 1;
            push @res, " <tr>\n";
            my $row = $data->[$i];
            for my $j (0..$#{$row}) {
                my $field_idx = $field_idxs[$j];
                my $align;
                if ($field_idx >= 0 && $tfa->[$field_idx]) {
                    $align = $tfa->[$field_idx];
                    $align = "right" if $align eq 'number';
                    $align = "middle" if $align eq 'center';
                }
                push @res, "  <$data_elem",
                    ($align ? " align=\"$align\"" : ""),
                    ">", HTML::Entities::encode_entities($row->[$j]),
                    "</$data_elem>\n";
            }
            push @res, " </tr>\n";
            push @res, "</thead>\n" if $i == 0;
        }
        push @res, "</tbody>\n";
        push @res, "</table>\n";
        join '', @res;
    } else {
        no warnings 'uninitialized';
        shift @$data if $header_row;
        join("", map {join("\t", @$_)."\n"} @$data);
    }
}

sub format {
    my ($res, $format, $is_naked, $cleanse) = @_;

    if ($format =~ /\A(text|text-simple|text-pretty|csv|html)\z/) {
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

# BEGIN FATPACK CODE: Data/Check/Structure.pm Text/Table/Any.pm Text/Table/Tiny.pm
{
    no strict 'refs';
    $main::fatpacked{"Data/Check/Structure.pm"} = '##line '.(1+__LINE__).' "'.__FILE__."\"\n".<<'DATA_CHECK_STRUCTURE';
#package Data::Check::Structure;
#
#our $DATE = '2014-07-14'; 
#our $VERSION = '0.03'; 
#
#use 5.010001;
#use strict;
#use warnings;
#
#require Exporter;
#our @ISA = qw(Exporter);
#our @EXPORT_OK = qw(
#                       is_aoa
#                       is_aoaos
#                       is_aoh
#                       is_aohos
#                       is_aos
#                       is_hoa
#                       is_hoaos
#                       is_hoh
#                       is_hohos
#                       is_hos
#               );
#
#sub is_aos {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'ARRAY';
#    for my $i (0..@$data-1) {
#        last if defined($max) && $i >= $max;
#        return 0 if ref($data->[$i]);
#    }
#    1;
#}
#
#sub is_aoa {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'ARRAY';
#    for my $i (0..@$data-1) {
#        last if defined($max) && $i >= $max;
#        return 0 unless ref($data->[$i]) eq 'ARRAY';
#    }
#    1;
#}
#
#sub is_aoaos {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'ARRAY';
#    my $aos_opts = {max=>$max};
#    for my $i (0..@$data-1) {
#        last if defined($max) && $i >= $max;
#        return 0 unless is_aos($data->[$i], $aos_opts);
#    }
#    1;
#}
#
#sub is_aoh {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'ARRAY';
#    for my $i (0..@$data-1) {
#        last if defined($max) && $i >= $max;
#        return 0 unless ref($data->[$i]) eq 'HASH';
#    }
#    1;
#}
#
#sub is_aohos {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'ARRAY';
#    my $hos_opts = {max=>$max};
#    for my $i (0..@$data-1) {
#        last if defined($max) && $i >= $max;
#        return 0 unless is_hos($data->[$i], $hos_opts);
#    }
#    1;
#}
#
#sub is_hos {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'HASH';
#    my $i = 0;
#    for my $k (keys %$data) {
#        last if defined($max) && ++$i >= $max;
#        return 0 if ref($data->{$k});
#    }
#    1;
#}
#
#sub is_hoa {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'HASH';
#    my $i = 0;
#    for my $k (keys %$data) {
#        last if defined($max) && ++$i >= $max;
#        return 0 unless ref($data->{$k}) eq 'ARRAY';
#    }
#    1;
#}
#
#sub is_hoaos {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'HASH';
#    my $i = 0;
#    for my $k (keys %$data) {
#        last if defined($max) && ++$i >= $max;
#        return 0 unless is_aos($data->{$k});
#    }
#    1;
#}
#
#sub is_hoh {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'HASH';
#    my $i = 0;
#    for my $k (keys %$data) {
#        last if defined($max) && ++$i >= $max;
#        return 0 unless ref($data->{$k}) eq 'HASH';
#    }
#    1;
#}
#
#sub is_hohos {
#    my ($data, $opts) = @_;
#    $opts //= {};
#    my $max = $opts->{max};
#
#    return 0 unless ref($data) eq 'HASH';
#    my $i = 0;
#    for my $k (keys %$data) {
#        last if defined($max) && ++$i >= $max;
#        return 0 unless is_hos($data->{$k});
#    }
#    1;
#}
#
#1;
#
#__END__
#
DATA_CHECK_STRUCTURE

    $main::fatpacked{"Text/Table/Any.pm"} = '##line '.(1+__LINE__).' "'.__FILE__."\"\n".<<'TEXT_TABLE_ANY';
#package Text::Table::Any;
#
#our $DATE = '2016-12-29'; 
#our $VERSION = '0.06'; 
#
#
#sub _encode {
#    my $val = shift;
#    $val =~ s/([\\"])/\\$1/g;
#    "\"$val\"";
#}
#
#sub table {
#    my %params = @_;
#
#    my $rows       = $params{rows} or die "Must provide rows!";
#    my $backend    = $params{backend} || 'Text::Table::Tiny';
#    my $header_row = $params{header_row} // 0;
#
#    if ($backend eq 'Text::Table::Tiny') {
#        require Text::Table::Tiny;
#        return Text::Table::Tiny::table(
#            rows => $rows, header_row => $header_row) . "\n";
#    } elsif ($backend eq 'Text::Table::TinyColor') {
#        require Text::Table::TinyColor;
#        return Text::Table::TinyColor::table(
#            rows => $rows, header_row => $header_row) . "\n";
#    } elsif ($backend eq 'Text::Table::TinyColorWide') {
#        require Text::Table::TinyColorWide;
#        return Text::Table::TinyColorWide::table(
#            rows => $rows, header_row => $header_row) . "\n";
#    } elsif ($backend eq 'Text::Table::TinyWide') {
#        require Text::Table::TinyWide;
#        return Text::Table::TinyWide::table(
#            rows => $rows, header_row => $header_row) . "\n";
#    } elsif ($backend eq 'Text::Table::Org') {
#        require Text::Table::Org;
#        return Text::Table::Org::table(
#            rows => $rows, header_row => $header_row);
#    } elsif ($backend eq 'Text::Table::CSV') {
#        require Text::Table::CSV;
#        return Text::Table::CSV::table(
#            rows => $rows);
#    } elsif ($backend eq 'Text::Table::HTML') {
#        require Text::Table::HTML;
#        return Text::Table::HTML::table(
#            rows => $rows, header_row => $header_row);
#    } elsif ($backend eq 'Text::Table::HTML::DataTables') {
#        require Text::Table::HTML::DataTables;
#        return Text::Table::HTML::DataTables::table(
#            rows => $rows, header_row => $header_row);
#    } elsif ($backend eq 'Text::ANSITable') {
#        require Text::ANSITable;
#        my $t = Text::ANSITable->new(
#            use_utf8 => 0,
#            use_box_chars => 0,
#            use_color => 0,
#            border_style => 'Default::single_ascii',
#        );
#        if ($header_row) {
#            $t->columns($rows->[0]);
#            $t->add_row($rows->[$_]) for 1..@$rows-1;
#        } else {
#            $t->columns([ map {"col$_"} 0..$#{$rows->[0]} ]);
#            $t->add_row($_) for @$rows;
#        }
#        return $t->draw;
#    } elsif ($backend eq 'Text::ASCIITable') {
#        require Text::ASCIITable;
#        my $t = Text::ASCIITable->new();
#        if ($header_row) {
#            $t->setCols(@{ $rows->[0] });
#            $t->addRow(@{ $rows->[$_] }) for 1..@$rows-1;
#        } else {
#            $t->setCols(map { "col$_" } 0..$#{ $rows->[0] });
#            $t->addRow(@$_) for @$rows;
#        }
#        return "$t";
#    } elsif ($backend eq 'Text::FormatTable') {
#        require Text::FormatTable;
#        my $t = Text::FormatTable->new(join('|', ('l') x @{ $rows->[0] }));
#        $t->head(@{ $rows->[0] });
#        $t->row(@{ $rows->[$_] }) for 1..@$rows-1;
#        return $t->render;
#    } elsif ($backend eq 'Text::MarkdownTable') {
#        require Text::MarkdownTable;
#        my $out = "";
#        my $fields =  $header_row ?
#            $rows->[0] : [map {"col$_"} 0..$#{ $rows->[0] }];
#        my $t = Text::MarkdownTable->new(file => \$out, columns => $fields);
#        foreach (($header_row ? 1:0) .. $#{$rows}) {
#            my $row = $rows->[$_];
#            $t->add( {
#                map { $fields->[$_] => $row->[$_] } 0..@$fields-1
#            });
#        }
#        $t->done;
#        return $out;
#    } elsif ($backend eq 'Text::Table') {
#        require Text::Table;
#        my $t = Text::Table->new(@{ $rows->[0] });
#        $t->load(@{ $rows }[1..@$rows-1]);
#        return $t;
#    } elsif ($backend eq 'Text::TabularDisplay') {
#        require Text::TabularDisplay;
#        my $t = Text::TabularDisplay->new(@{ $rows->[0] });
#        $t->add(@{ $rows->[$_] }) for 1..@$rows-1;
#        return $t->render . "\n";
#    } else {
#        die "Unknown backend '$backend'";
#    }
#}
#
#1;
#
#__END__
#
TEXT_TABLE_ANY

    $main::fatpacked{"Text/Table/Tiny.pm"} = '##line '.(1+__LINE__).' "'.__FILE__."\"\n".<<'TEXT_TABLE_TINY';
#use 5.006;
#use strict;
#use warnings;
#package Text::Table::Tiny;
#$Text::Table::Tiny::VERSION = '0.04';
#use parent 'Exporter';
#use List::Util qw();
#
#our @EXPORT_OK = qw/ generate_table /;
#
#
#
#our $COLUMN_SEPARATOR = '|';
#our $ROW_SEPARATOR = '-';
#our $CORNER_MARKER = '+';
#our $HEADER_ROW_SEPARATOR = '=';
#our $HEADER_CORNER_MARKER = 'O';
#
#sub generate_table {
#
#    my %params = @_;
#    my $rows = $params{rows} or die "Must provide rows!";
#
#    my $widths = _maxwidths($rows);
#    my $max_index = _max_array_index($rows);
#
#    my $format = _get_format($widths);
#    my $row_sep = _get_row_separator($widths);
#    my $head_row_sep = _get_header_row_separator($widths);
#
#    my @table;
#    push @table, $row_sep;
#
#    my $data_begins = 0;
#    if ( $params{header_row} ) {
#        my $header_row = $rows->[0];
#        $data_begins++;
#        push @table, sprintf(
#                         $format, 
#                         map { defined($header_row->[$_]) ? $header_row->[$_] : '' } (0..$max_index)
#                     );
#        push @table, $params{separate_rows} ? $head_row_sep : $row_sep;
#    }
#
#    foreach my $row ( @{ $rows }[$data_begins..$#$rows] ) {
#        push @table, sprintf(
#	    $format, 
#	    map { defined($row->[$_]) ? $row->[$_] : '' } (0..$max_index)
#	);
#        push @table, $row_sep if $params{separate_rows};
#    }
#
#    push @table, $row_sep unless $params{separate_rows};
#    return join("\n",grep {$_} @table);
#}
#
#sub _get_cols_and_rows ($) {
#    my $rows = shift;
#    return ( List::Util::max( map { scalar @$_ } @$rows), scalar @$rows);
#}
#
#sub _maxwidths {
#    my $rows = shift;
#    my $max_index = _max_array_index($rows);
#    my $widths = [];
#    for my $i (0..$max_index) {
#        my $max = List::Util::max(map {defined $$_[$i] ? length($$_[$i]) : 0} @$rows);
#        push @$widths, $max;
#    }
#    return $widths;
#}
#
#sub _max_array_index {
#    my $rows = shift;
#    return List::Util::max( map { $#$_ } @$rows );
#}
#
#sub _get_format {
#    my $widths = shift;
#    return "$COLUMN_SEPARATOR ".join(" $COLUMN_SEPARATOR ",map { "%-${_}s" } @$widths)." $COLUMN_SEPARATOR";
#}
#
#sub _get_row_separator {
#    my $widths = shift;
#    return "$CORNER_MARKER$ROW_SEPARATOR".join("$ROW_SEPARATOR$CORNER_MARKER$ROW_SEPARATOR",map { $ROW_SEPARATOR x $_ } @$widths)."$ROW_SEPARATOR$CORNER_MARKER";
#}
#
#sub _get_header_row_separator {
#    my $widths = shift;
#    return "$HEADER_CORNER_MARKER$HEADER_ROW_SEPARATOR".join("$HEADER_ROW_SEPARATOR$HEADER_CORNER_MARKER$HEADER_ROW_SEPARATOR",map { $HEADER_ROW_SEPARATOR x $_ } @$widths)."$HEADER_ROW_SEPARATOR$HEADER_CORNER_MARKER";
#}
#
#*table = \&generate_table;
#
#1;
#
#__END__
#
#
TEXT_TABLE_TINY

    $main::fatpacked{$_} =~ s/^\#//mg for ('Data/Check/Structure.pm', 'Text/Table/Any.pm', 'Text/Table/Tiny.pm');
    my $class = 'FatPacked::'.(0+\%main::fatpacked);
    unless (defined &{"${class}::INC"}) { if ($] < 5.008) { *{"${class}::INC"} = sub { if (my $fat = $_[0]{$_[1]}) { return sub { return 0 unless length $fat; $fat =~ s/^([^\n]*\n?)//; $_ = $1; return 1; }; } return; }; } else { *{"${class}::INC"} = sub { if (my $fat = $_[0]{$_[1]}) { open my $fh, '<', \$fat or die "FatPacker error loading $_[1] (could be a perl installation issue?)"; return $fh; } return; }; } }
    my $hook = bless(\%main::fatpacked, $class);
    push @INC, $hook unless grep {ref($_) && "$_" eq "$hook"} @INC;
}
# END OF FATPACK CODE

1;
# ABSTRACT: Format enveloped result

=for Pod::Coverage ^(firstidx)$

=head1 SYNOPSIS


=head1 DESCRIPTION


=head1 FUNCTIONS

=head2 format($res, $format[ , $is_naked=0, $cleanse=1 ]) => str


=head1 ENVIRONMENT

=head2 FORMAT_PRETTY_TABLE_BACKEND => str

If this is set, will render text table using L<Text::Table::Any> (with
C<backend> set to the value of this environment variable) instead of the default
L<Text::Table::Tiny>. This is useful if you want to output text table in a
different format, for example to generate Org tables (make sure
L<Text::Table::Org> backend is already installed):

 % FORMAT_PRETTY_TABLE_BACKEND=Text::Table::Org lcpan rdeps Getopt::Lucid

=head2 FORMAT_PRETTY_TABLE_COLUMN_ORDERS => array (json)

Set the default of C<table_column_orders> in C<format_options> in result
metadata, similar to what's implemented in L<Perinci::Result::Format> and
L<Data::Format::Pretty::Console>.


=head1 SEE ALSO

L<Perinci::Result::Format>, a more heavyweight version of this module.

L<Perinci::CmdLine::Lite> uses this module to format enveloped result.

=cut
