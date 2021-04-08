#!/usr/bin/perl

# install-cgi.pl - install renamed copies of cgi-kqml-bridge.pl with the appropriate addresses for those names and the current hostname
# 2017-04-03
# William de Beaumont

use strict 'vars';

@ARGV == 2 or die "USAGE: ./install-cgi.pl cgi-dir script-name";
my ($cgi_dir, $script_name) = @ARGV;

my $default_address = '127.0.0.1:6200';
my %tiu_addresses = qw(
  drum		127.0.0.1:6201
  cabot		127.0.0.1:6202
  step		127.0.0.1:6203
  web-tools	127.0.0.1:6204
  parse		127.0.0.1:6204
  get-word-def	127.0.0.1:6204
  lex-ont	127.0.0.1:6243
  bob		127.0.0.1:6205
  cogent	127.0.0.1:6206
  propolis	127.0.0.1:6207
  cwms		127.0.0.1:6208
  cwmsreader	127.0.0.1:6209
  drum-dev	127.0.0.1:6241
  drum-er	127.0.0.1:6241
  step-dev	127.0.0.1:6243
);

my $address = $default_address;
$address = $tiu_addresses{$script_name} if (`hostname` eq "trips.ihmc.us\n");

my ($host, $port) = split(/:/, $address);

open CKB, "<cgi-kqml-bridge.pl" or die "Can't open cgi-kqml-bridge.pl: $!";
local $/;
my $ckb = <CKB>;
close CKB;

$ckb =~ s/PeerAddr => '[^']*'/PeerAddr => '$host'/g;
$ckb =~ s/PeerPort => \d*/PeerPort => $port/g;

open SCRIPT, ">$cgi_dir/$script_name" or die "Can't open $script_name: $!";
print SCRIPT $ckb;
close SCRIPT;
chmod 0755, "$cgi_dir/$script_name" or die "Can't chmod $script_name: $!";

