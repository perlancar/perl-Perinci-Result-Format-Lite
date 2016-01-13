#!perl

use 5.010;
use strict;
use warnings;
use Test::More 0.98;

use Perinci::Result::Format::Lite;

my $fmt = \&Perinci::Result::Format::Lite::format;

like($fmt->([200, "OK", {}], 'foo'), qr/\{\}/, "unknown format -> fallback to json-pretty");

subtest "text-simple" => sub {
    is($fmt->([200, "OK"], 'text-simple'), "");
    is($fmt->([200, "OK", "a"], 'text-simple'), "a\n", "newline appended");
    is($fmt->([200, "OK", "a\n"], 'text-simple'), "a\n", "newline already exists so not added");
    is($fmt->([404, "Not found", "a\n"], 'text-simple'), "ERROR 404: Not found\n", "error");

    # XXX test: hash -> 2-column table
    # XXX test: aos -> table
    # XXX test: aoaos -> table
    # XXX test: aohos -> table
    # XXX test: table_column_orders
};

subtest "text-pretty" => sub {
    like($fmt->([200, "OK", {a=>1}], 'text-pretty'), qr/key[ ]*\|[ ]*value.+a[ ]*\|[ ]*1/s, "hash");

    # XXX test: aos -> table
    # XXX test: aoaos -> table
    # XXX test: aohos -> table
    # XXX test: table_column_orders
};

subtest "json-pretty" => sub {
    like($fmt->([200, "OK", [1,2]], 'json-pretty'),
         qr/\[\s*
            200,\s*
            "OK",\s*
            \[\s*1,\s*2\s*\]
            (,\s*\{\})?
            \s*\]/sx);
};

# XXX test: opt:naked=1
# XXX test: opt:cleanse=0

done_testing();
