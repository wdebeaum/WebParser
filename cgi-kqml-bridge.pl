#!/usr/bin/perl -T -I.

# cgi-kqml-bridge.pl - CGI script that simply sends the HTTP request on to the
# Facilitator in KQML, waits for a response, and converts it back to HTTP
# 2014-07-10
# William de Beaumont

use CGI qw/:standard :cgi-lib/;
use IO::Socket;
use KQML;
use strict vars;

sub quote_for_kqml {
  $_ = shift @_;
  s/[\\"]/\\$&/g;
  qq/"$_"/;
}

# convert CGI variables to KQML messages
my $request_method = request_method();
my $request_path = script_name() . path_info(); # '"/parse"';
$request_path =~ s/\?.*//s # work around bug in Perl 5.10's CGI.pm
  if (!$^V or $^V lt v5.16.0); # don't actually know which version fixed it
$request_path = quote_for_kqml($request_path);
my %params = Vars();
my $query = '';
for my $key (keys %params) {
  $query .= " :$key " . quote_for_kqml($params{$key});
}
$query = ":query ($query)" unless ($query eq '');
my $reply_id = "web" . int(rand(10000));
my $kqml_out = <<EOKQML;
(register :name $reply_id)
(tell :content (module-status ready))
(request :content (http $request_method $request_path $query) :reply-with $reply_id)
EOKQML

# open socket and write our KQML to it
my $sock =
  IO::Socket::INET->new(
    Proto => 'tcp',
    # NOTE: the following two lines are changed by install-cgi.pl
    PeerAddr => '127.0.0.1',
    PeerPort => 6200
  );
die "can't connect to facilitator: $!\n" unless ($sock);
$sock->autoflush(1);
print $sock $kqml_out;

# read from socket until we get a reply
my $kqml_in;
until (defined($kqml_in)) {
  my $msglist = KQML::KQMLRead($sock);
  if (ref($msglist) ne 'ARRAY') {
    print header(-type => 'text/plain'), "KQMLRead error $msglist";
    close($sock);
    exit 1;
  }
  my $msg = KQML::KQMLKeywordify($msglist);
  lc($msg->{':in-reply-to'}) eq $reply_id or next;
  if (lc($msg->{'verb'}) eq 'sorry') {
    if (KQML::KQMLAtomIsString($msg->{':comment'})) {
      print header(-type => 'text/plain'),
            KQML::KQMLStringAtomAsPerlString($msg->{':comment'});
    }
    close($sock);
    exit 1;
  }
  ref($msg->{':content'}) eq 'ARRAY' or next;
  $kqml_in = KQML::KQMLKeywordify($msg->{':content'});
}
close($sock);

# convert KQML reply to HTTP
lc($kqml_in->{'verb'}) eq 'http' or die "non-http reply";
KQML::KQMLAtomIsString($kqml_in->{':content'}) or die "http content must be a string";
my $content = KQML::KQMLStringAtomAsPerlString($kqml_in->{':content'});
my $status = $kqml_in->{'list'}[1];
my @headers = (-status => $status);
for my $key (keys %$kqml_in) {
  if ($key =~ /^:([\w-]+)$/ and $key ne ':content') {
    my $dash_key = '-' . $1;
    my $val = $kqml_in->{$key};
    $val = KQML::KQMLStringAtomAsPerlString($val) if (KQML::KQMLAtomIsString($val));
    push @headers, $dash_key, $val;
  }
}
print header(@headers), $content;

