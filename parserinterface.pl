#!/usr/bin/perl

# parserinterface.pl - web interface to the parser (wrapper for web-parser.cgi that adds the submission form and colorizes and linkifies the output)

use strict vars;

my @colors = qw(00BBFF 00FF00 FF0000 CB00CE 2BDD00 0092CC 95CC00 FFAA00 8700FF A5700D BB7474 75C440 6A8DB7);
my $currentColor = 0;
my %id2color = ();

sub getIdColor
{
  my $id = shift;
  unless (exists($id2color{$id}))
  {
    if ($currentColor < @colors)
    {
      $id2color{$id} = $colors[$currentColor++];
    } else
    {
      $id2color{$id} = sprintf("%02X%02X%02X", int(rand(100)), int(rand(100)), int(rand(100)));
    }
  }
  return $id2color{$id};
}

# check if we're sending to Internet Explorer (browsers that spoof MSIE deserve what they get)
my $usesvg = 1;
$usesvg = 0 if ($ENV{HTTP_USER_AGENT} =~ /MSIE/);

if ($ENV{REQUEST_METHOD} ne 'GET')
{
  die "wrong request method\n";
}

my %query = ();
for my $assgn (split(/&/, $ENV{'QUERY_STRING'}))
{
  my ($name, $val) = split(/=/, $assgn);
  $name =~ s/\%([\dA-Fa-f]{2})/chr(hex($1))/ge;
  $name =~ s/\+/ /g;
  $val =~ s/\%([\dA-Fa-f]{2})/chr(hex($1))/ge;
  $val =~ s/\+/ /g;
  $query{lc($name)} = lc($val);
}

# header
if ($usesvg)
{
  print "Content-type: application/xhtml+xml\r\n\r\n";
  print <<EOP;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
EOP
} else
{
  print "Content-type: text/html\r\n\r\n<html>";
EOP
}

my $escaped_input = $query{input};
$escaped_input =~ s/&/&amp;/g;
$escaped_input =~ s/</&lt;/g;
$escaped_input =~ s/>/&gt;/g;
$escaped_input =~ s/"/&quot;/g;

print <<EOP;
<head>
 <meta http-equiv="content-type" content="text/html; charset=UTF-8"/>
 <title>TRIPS Parser On The Web</title>
 <meta name="description" content="Online access to the TRIPS parser"/>

 <style type="text/css">
  td.node {
    border-bottom: solid;
    text-align: center;
    font-weight: bold;
  }
  td.inlineNode {
    text-align: center;
    font-weight: bold;
  }
  td.prunedNode {
    text-align: center;
    font-weight: bold;
  }
  td.leaf {
    text-align: center;
    font-style: italic;
  }

  td.LinGONode {
    border-top: solid;
    border-right: solid;
    border-left: solid;
    border-width: thin;
    margin-right: 1px;
    margin-left: 1px;
    text-align: center;
  }
  td.LinGOLeaf {
    border-right: solid;
    border-left: solid;
    border-width: thin;
    margin-right: 2px;
    margin-left: 2px;
    text-align: center;
    font-style: italic;
  }
 </style>
</head>
<body>
<h1><img src="/images/monalogo_thin.jpg" alt="URCS (logo)" /> TRIPS Parser Web Interface</h1>
<form action="parserinterface.pl">
<div>
<input type="text" size="40" name="input" value="$escaped_input" />
<input type="submit" value="Parse" />
</div>
<!-- /form -->
EOP

if (exists($query{input}))
{
  # get the output of (lf)
  my @response = `./web-parser.cgi`;
  # get rid of extraneous Content-Type lines
  shift @response;

  # pretty-print it in HTML
  print "<pre>\n";
  my $stillInWordList = 0;
  for my $line (@response)
  {
    # skip blank lines
    next if ($line =~ /^\s*$/);

    # go to next step if we hit the parse tree
    last if ($line =~ /^;;;;; parse tree:/);

    # escape <>&
    $line =~ s/&/&amp;/g;
    $line =~ s/</&lt;/g;
    $line =~ s/>/&gt;/g;
    
    # linkify all words in the term-initial word list
    if ($line =~ /^;;;;;/ or $stillInWordList)
    {
      $line =~ s!W::([^\s\(\)]+)!<a href="../../lexicon/data/W%3A%3A\L$1\E.xml">W::$1</a>!g;
      if ($line =~ /\)\s*$/)
      {
	$stillInWordList = 0;
      } else
      {
        $stillInWordList = 1;
      }
    }

    # linkify LFs and Ws in terms
    $line =~ s!\(:\* ONT::([^\s\(\)]+) W::([^\s\(\)]+)\)!(:* <a href="../../lexicon/cgi/browseontology-ajax?search=$1#highlight">ONT::$1</a> <a href="../../lexicon/data/W%3A%3a\L$2\E.xml">W::$2</a>)!g;
    $line =~ s!V(\d+) ONT::([^\s\(\)]+)!V$1 <a href="../../lexicon/cgi/browseontology-ajax?search=$2#highlight">ONT::$2</a>!g;

    # colorize term IDs
    $line =~ s!V(\d+)!"<b style=\"background: #" . getIdColor($1) . "\">$&</b>"!ge;

    # convert ;s to horizontal rule
    $line =~ s!;;;;; !</pre><hr /><pre>!;

    print $line;
  }
  print "</pre>\n";
  
  print "<hr />\n";

  print <<EOP;
<!-- form -->
<script type="text/javascript">
/*<![CDATA[*/
// set whether the node is displayed
function setDisplay(node, displayOn)
{
  node.style.display = displayOn? "" : "none"
}

function setAllDisplay()
{
  var format = document.getElementById('format').value
  var contents = document.getElementById('contents').value
  var id = format + '-' + contents
  var trees = document.getElementById('trees').childNodes
  for (i = 0; i < trees.length; i++)
  {
    if (trees[i].nodeType != Node.TEXT_NODE)
    {
      setDisplay(trees[i], trees[i].id == id)
    }
  }
}
/*]]>*/
</script>
EOP
  my %treeformatselected = ('lingo' => 'selected="selected"');
  my %treecontentsselected = ('phrase' => 'selected="selected"');
  %treeformatselected = ($query{treeformat} => 'selected="selected"')
    if (exists($query{treeformat}));
  %treecontentsselected = ($query{treecontents} => 'selected="selected"')
    if (exists($query{treecontents}));
  print <<EOP;
<p>Format:
<select id="format" name="treeformat" onchange="setAllDisplay()">
<option value="lisp" $treeformatselected{lisp}>Lisp</option>
EOP
print qq(<option value="svg" $treeformatselected{svg}>SVG diagram</option>\n)
  if ($usesvg);
print <<EOP;
<option value="table" $treeformatselected{table}>HTML table diagram</option>
<option value="LinGO" $treeformatselected{lingo}>LinGO-like table diagram</option>
</select>
Contents:
<select id="contents" name="treecontents" onchange="setAllDisplay()">
<option value="full" $treecontentsselected{full}>Full tree</option>
<option value="phrase" $treecontentsselected{phrase}>Phrase-level nodes only</option>
</select>
</p>
</form>
<div id="trees">
EOP

  # translate parse tree into perl list-of-lists
  # (this is just a little evil)
  my $tree = join('', @response);
  $tree =~ s/^.*;;;;; parse tree:\n//s;
  $tree =~ s/[^\(\)\s]+/'$&'/g;
  $tree =~ tr/()/[]/;
  $tree =~ s/'\s+'/', '/g;
  $tree =~ s/'\s+\[/', [/g;
  $tree =~ s/\]\s+'/], '/g;
  $tree =~ s/\]\s+\[/], [/g;
  $tree = eval($tree);

  # get phrase tree

  my @phraseNodes = qw(UTT NP VP ADJP ADVBL PP CP);
  sub getPhraseNodesOnly
  { # returns a list of subtrees, but only one if the original root was a phrase node
    my $tree = shift;
    my $ret = undef;
    unless (ref($tree) eq "ARRAY")
    {
      $ret = [];
    } elsif (grep {$_ eq $tree->[0]} @phraseNodes)
    {
      $ret = [[$tree->[0], map { @{getPhraseNodesOnly($_)} } @{$tree}[1..$#{$tree}] ]];
    } else
    {
      $ret = [ map { @{getPhraseNodesOnly($_)} } @{$tree}[1..$#{$tree}] ];
    }
    return $ret;
  }

  my $phraseTree = getPhraseNodesOnly($tree->[0]);

  # print in different formats
  
  sub tree2lisp
  { # returns a tree in lisp format
    my $tree = shift;
    if (ref($tree) eq "ARRAY")
    {
      return '(' . join(' ', map { tree2lisp($_) } @$tree) . ')';
    } else
    {
      return $tree;
    }
  }

  my $nexttreeid = 1;
  sub tree2dot
  { # translate list-of-lists into Graphviz dot format
    my ($tree, $id) = @_;
    if (ref($tree) eq 'ARRAY')
    {
      my $rootname = $tree->[0];
      return "  \"$id\" [label=\"$rootname\"]\n" if (@$tree == 1);
      my $ret = "  \"$id\" [label=\"$rootname\",rank=\"sink\"]\n";
      for my $child (@{$tree}[1..$#{$tree}])
      {
	my $childid = $nexttreeid;
	$nexttreeid++;
	$ret .= "  \"$id\" -> \"$childid\"\n" . tree2dot($child, $childid);
	#$ret .= "  \"$childid\" -> \"$id\"\n" . tree2dot($child, $childid);
      }
      return $ret;
    } else # string
    {
      return "  \"$id\" [label=\"\Q$tree\E\"]\n";
    }
  }

  sub tree2tableRecursive
  { # returns the column offset of the middle of the root node
    my ($tree, $rows, $currentRow) = @_;
    if (ref($tree) eq "ARRAY")
    {
      my $rootname = $tree->[0];
      if (@$tree > 1)
      {
	my $nextRow = $currentRow+1;
	my $padspan = scalar(@{$rows->[$currentRow]}) -
	              scalar(@{$rows->[$nextRow]});
	if ($padspan > 0)
	{
	  push @{$rows->[$nextRow]}, (qq(<td colspan="$padspan" class="alignpad"></td>), ('')x($padspan-1));
	}
	my $firstLength = scalar(@{$rows->[$nextRow]});
	my $firstChildMiddle = undef;
	my $lastChildMiddle = undef;
	for my $child (@{$tree}[1..$#{$tree}])
	{
	  $lastChildMiddle = tree2tableRecursive($child, $rows, $nextRow);
	  unless (defined($firstChildMiddle))
	  {
	    $firstChildMiddle = $lastChildMiddle;
	  }
	}
	my $lastLength = scalar(@{$rows->[$nextRow]});
	my $colspan1 = $firstChildMiddle - $firstLength;
	my $colspan2 = $lastChildMiddle - ($firstLength + $colspan1);
	my $class = "node";
	if (@$tree <= 2)
	{
	  $class = "inlineNode";
	  my $length = $lastLength - $firstLength;
	  if ($length == 1)
	  {
	    push @{$rows->[$currentRow]}, qq(<td class="inlineNode" colspan="1">$rootname</td>);
	    return $firstLength;
	  } elsif ($length == 2)
	  {
	    push @{$rows->[$currentRow]}, (qq(<td class="inlineNode" colspan="2">$rootname</td>), '');
	    return $firstLength+1;
	  } else
	  {
	    $colspan1 = $firstChildMiddle - $firstLength - 1;
	    $colspan2 = 2;
	  }
	}
	my $colspan3 = $lastLength - ($firstLength + $colspan1 + $colspan2);
	push @{$rows->[$currentRow]}, qq(<td colspan="$colspan1" class="leftpad"/>)
	  if ($colspan1 > 0);
	push @{$rows->[$currentRow]}, qq(<td class="$class" colspan="$colspan2">$rootname</td>);
	push @{$rows->[$currentRow]}, ('')x($colspan2-1)
	  if ($colspan2 > 1);
	push @{$rows->[$currentRow]}, qq(<td colspan="$colspan3" class="rightpad"/>)
	  if ($colspan3 > 0);
	return $firstLength + $colspan1 + ($colspan2 / 2);
      } else
      {
        push @{$rows->[$currentRow]}, (qq(<td class="prunedNode" colspan="2">$rootname</td>), '');
	return $#{$rows->[$currentRow]} - 1;
      }
    } else
    {
      push @{$rows->[$currentRow]}, (qq(<td class="leaf" colspan="2">$tree</td>), '');
      return $#{$rows->[$currentRow]} - 1;
    }
  }

  sub tree2table
  { # translate list-of-lists into HTML table tree diagram
    my $tree = shift;
    my $rows = [[]];
    tree2tableRecursive($tree, $rows, 0);
    my $maxcols = [sort {$a<=>$b} map {scalar(@$_)} @$rows]->[-1];
    unshift @$rows, [("<td> </td>")x($maxcols)];
    return '<tr>' . join('</tr><tr>', map {join('',@$_)} @$rows) . '</tr>';
  }

  sub tree2LinGORecursive
  {
    my ($tree, $rows, $currentRow) = @_;
    if (ref($tree) eq "ARRAY")
    {
      my $rootname = $tree->[0];
      if (@$tree > 1)
      {
	my $nextRow = $currentRow+1;
	my $padspan = scalar(@{$rows->[$currentRow]}) -
	              scalar(@{$rows->[$nextRow]});
	if ($padspan > 0)
	{
	  push @{$rows->[$nextRow]}, (qq(<td colspan="$padspan" class="alignpad"></td>), ('')x($padspan-1));
	}
	my $firstLength = scalar(@{$rows->[$nextRow]});
	for my $child (@{$tree}[1..$#{$tree}])
	{
	  tree2LinGORecursive($child, $rows, $nextRow);
	}
	my $lastLength = scalar(@{$rows->[$nextRow]});
	my $colspan = $lastLength - $firstLength -1;
	push @{$rows->[$currentRow]}, (qq(<td class="LinGONode" colspan="$colspan">$rootname</td>), qq(<td> </td>), ('')x($colspan-1));
      } else
      {
        push @{$rows->[$currentRow]}, (qq(<td class="LinGONode">$rootname</td>), qq(<td> </td>));
      }
    } else
    {
      push @{$rows->[$currentRow]}, (qq(<td class="LinGOLeaf">$tree</td>), qq(<td> </td>));
    }
  }

  sub tree2LinGO
  { # translate list-of-lists into HTML table tree diagram like LinGO's
    my $tree = shift;
    my $rows = [[]];
    tree2LinGORecursive($tree, $rows, 0);
    my $maxcols = [sort {$a<=>$b} map {scalar(@$_)} @$rows]->[-1];
    unshift @$rows, [("<td> </td>")x($maxcols)];
    return '<tr>' . join('</tr><tr>', map {join('',@$_)} @$rows) . '</tr>';
  }
 
  sub printTree
  {
    my ($contents, $tree) = @_;

    print qq(<pre id="lisp-$contents" name="tree">\n);
    print tree2lisp($tree);
    print "</pre>\n";

    if ($usesvg)
    {
      print qq(<div id="svg-$contents" name="tree">\n);
      open DOT2SVG, "|/u/www/research/cisd/projects/trips/parser/graphviz/bin/dot -Tsvg |grep '^<svg' -A100000 |sed -e 's/font-weight:regular/font-weight:normal/g'" or die "Can't run dot to translate parse tree to SVG: $!\n";
      print DOT2SVG qq(digraph ParseTree {\n  graph [nodesep="0.1",ranksep="0.1"]\n  node [fontsize="10.0",shape="none",margin="0,0", height="0"]\n  edge [arrowhead="none"]\n) . tree2dot($tree,0) . "}\n";
      close DOT2SVG;
      print "</div>\n";
    }

    print qq(<table id="table-$contents" name="tree">\n);
    print tree2table($tree);
    print "</table>\n";
    
    print qq(<table id="LinGO-$contents" name="tree" cellspacing="0">\n);
    print tree2LinGO($tree);
    print "</table>\n";
  }

  printTree("full", $tree->[0]);
  printTree("phrase", $phraseTree->[0]);

  print <<EOP;
</div>
<script type="text/javascript">
/*<![CDATA[*/
setAllDisplay()
/*]]>*/
</script>
EOP
  
} else
{
  print "</form>\n";
}

# footer
print <<EOP;
<hr />
<div><a href="../../lexicon/browse-ont-lex.html">Browse the TRIPS Lexicon and Ontology</a></div>
<div><a href="../logical-form/lfdoc.pdf">LF Documentation (pdf)</a></div>
<div>Parser built on 
EOP
print scalar(localtime([stat("../etc/lisp/web-parser.image")]->[9]));
print <<EOP;
</div>
<p>Development of this system has been supported in part by The National Science Foundation, with grants 0958193 and 1012205, The Office of Naval Research, under grant N000141110417.</p>
</body></html>
EOP

