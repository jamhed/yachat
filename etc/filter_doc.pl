#!/usr/bin/perl
use strict;
# sample:
# %msg group list
# msg(M = <<"conv/users">>, [UserId, ConvId]) when is_number(UserId), is_number(ConvId) ->
# [M, ok]

my $fun;

while(<>) {
    if (s/%msg\s+(.*)//) {
        print "\nh3. ", ucfirst $1;
        print "\n\n";
        next;
    }
    if (/msg.*\<\"(.*?)\"\>.*\[(.*)\]/ or /msg.*\<\"(.*?)\"\>\>\,\s+(.*)\)/) {
        print "MSG: ";
        $fun = $1;
        if ($2) {
            print "[\"$1\", $2]\n" 
        } else {
            print "[\"$1\"]\n" 
        }
        print "\n";
        next;
    }
    if (/(\[M.*\])/) {
        my $re = $1;
        $re =~ s/M/\"$fun\"/;
        print "RET: ", $re, "\n";
    }
}

