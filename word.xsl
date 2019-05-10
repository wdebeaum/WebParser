<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html" 
    doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
    doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" />

<xsl:template match="/WORDS">
 <html>
 <head>
 <title>TRIPS Word Lookup: <xsl:value-of select="@name" /></title>
 <meta name="date" scheme="RFC822" content="{@modified}" />
 <style type="text/css">
  .wn {
    <xsl:if test="@show-wn-senses-only-for-core-synsets">
      display: none;
    </xsl:if>
  }
 </style>
 <style type="text/css">ul { padding-left: 1em; }</style>

<!-- jQuery autocomplete -->
  <script src="../jquery/jquery-latest.js"></script>
  <link rel="stylesheet" href="../jquery/jquery.autocomplete.css" type="text/css" />
  <script type="text/javascript" src="../jquery/jquery.bgiframe.min.js"></script>
  <script type="text/javascript" src="../jquery/jquery.dimensions.js"></script>
  <script type="text/javascript" src="../jquery/jquery.autocomplete.js"></script>
  <script>
<![CDATA[
  $(document).ready(function(){
    $("#search-input").autocomplete("lex-ont?side=lex&ret=autocomplete");
  });
]]>
  </script>
<!-- end jQuery stuff -->

 <script type="text/javascript">
<![CDATA[

var inFrames = false

// for hide/show components
function toggleVisible(id, linktext)
{
  var span = document.getElementById(id)
  var link = document.getElementById(id + '-link')
  if (span.style.display == "none")
  {
    link.innerHTML = link.innerHTML.replace("show", "hide")
    span.style.display = null
    if (!span.innerHTML.match("<"))
    {
      if (id.match("-synset"))
      {
	span.innerHTML = linkifyWords(span.innerHTML)
      } else
      {
	span.innerHTML = linkifyAncestors(span.innerHTML)
      }
    }
  } else
  {
    link.innerHTML = link.innerHTML.replace("hide", "show")
    span.style.display = "none"
  }
}

function setClassVisible(cn, visible) {
  var sel = '.' + cn;
  var rules = document.styleSheets[0].cssRules;
  for (var i = 0; i < rules.length; i++) {
    if ((rules[i] instanceof CSSStyleRule) &&
        rules[i].selectorText == sel) {
      rules[i].style.display = (visible ? null : "none");
      break;
    }
  }
}

// turn a comma-separated list of words into a list of links to their data files
function linkifyWords(wordstring)
{
  var words = wordstring.split(',')
  var linkstring = ""
  for (i in words)
  {
    linkstring += "<a href=\"lex-ont?side=lex&amp;q=" + encodeURIComponent(words[i]) + "\">" + words[i] + "</a>, "
  }
  return linkstring + "<br />"
}

function fixSenseKey(sk) {
  return sk.replace(
    /-([1-5])-(\d{2})-(\d{2})-(?:-|(\w+)-(\d{2}))$/,
    '%$1:$2:$3:$4:$5'
  );
}

// same as above for ancestor ONTs
function linkifyAncestors(ancestorstring)
{
  var ancestors = ancestorstring.split(',')
  var linkstring = "<br />"
  var endstring = ""
  for (i in ancestors)
  {
    linkstring += "<ul><li><a href=\"lex-ont?side=ont&amp;q=" + ancestors[i] + "#highlight\"" + (inFrames? 'target="ontology"' : '') + ">" + fixSenseKey(ancestors[i]) + "</a>, "
    endstring += "</li></ul>"
  }
  return linkstring + endstring
}

function setTargets() {
  var links = document.getElementsByTagName("a")
  if (top.location.href != window.location.href) { // we're in frames
    inFrames = true
    for (var i = 0; i < links.length; i++) {
      if (/lex-ont\?side=ont/.test(links[i].href)) {
        links[i].target = "ontology"
      }
    }
  }
  // put WN sense keys back to their proper format
  for (var i = 0; i < links.length; i++) {
    var oldHTML = links[i].innerHTML;
    var newHTML = fixSenseKey(oldHTML);
    if (oldHTML != newHTML) {
      links[i].innerHTML = newHTML;
    }
  }
}

]]>
 </script>
 </head>
 <body onload="setTargets()">
 <h1>TRIPS+WN Word Lookup</h1>
 <form action="#">
  <input type="hidden" name="side" value="lex" />
  <input type="text" size="40" name="q" id="search-input" value="{@q}" />
  <input type="submit" value="Look Up" /><br/>
  <label>
   <input type="checkbox" name="use-trips-and-wf-senses" value="t">
    <xsl:if test="@use-trips-and-wf-senses">
     <xsl:attribute name="checked">checked</xsl:attribute>
    </xsl:if>
   </input>
   Find mapped WN senses even for POS with native TRIPS senses
  </label><br/>
  <label>
   <input type="checkbox" name="show-wn-senses-only-for-core-synsets" value="t" onchange="setClassVisible('wn', !this.checked)">
    <xsl:if test="@show-wn-senses-only-for-core-synsets">
     <xsl:attribute name="checked">checked</xsl:attribute>
    </xsl:if>
   </input>
   Show WN senses only for core synsets
  </label>
 </form>
 <xsl:if test="@allow-editing">
  <form action="lex-ont-edit">
   <input type="hidden" name="op" value="download" />
   <input type="submit" value="download edits" />
  </form>
 </xsl:if>
 <xsl:for-each select="WORD">
  <h2><xsl:value-of select="@name" /></h2>
  <ul>
   <xsl:for-each select="POS">
    <xsl:variable name="pos" select="@name" />
    <li>
    <xsl:choose> <!-- map abbreviated parts-of-speech to their spelled-out versions -->
     <xsl:when test="$pos = 'adj'">Adjective</xsl:when>
     <xsl:when test="$pos = 'adv'">Adverb</xsl:when>
     <xsl:when test="$pos = 'art'">Article</xsl:when>
     <xsl:when test="$pos = 'conj'">Conjunction</xsl:when>
     <xsl:when test="$pos = 'fp'">Filled-Pause</xsl:when>
     <xsl:when test="$pos = 'n'">Noun</xsl:when>
     <xsl:when test="$pos = 'prep'">Preposition</xsl:when>
     <xsl:when test="$pos = 'pro'">Pronoun</xsl:when>
     <xsl:when test="$pos = 'punc'">Punctuation</xsl:when>
     <xsl:when test="$pos = 'quan'">Quantifier</xsl:when>
     <xsl:when test="$pos = 'v'">Verb</xsl:when>
     <xsl:otherwise><xsl:value-of select="$pos" /></xsl:otherwise>
    </xsl:choose> Classes:<br />
     <dl style="padding-left: 2.0em">
      <xsl:for-each select="MORPH[@cat='nom' and @from]">
       <dt>Nominalization of
	<a href="lex-ont?side=lex&amp;q={@from}"><xsl:value-of select="@from" /></a>
       </dt>
      </xsl:for-each>
      <xsl:for-each select="CLASS">
       <dt class="{@source}">
	<a href="lex-ont?side=ont&amp;q={@onttype}#highlight" style="color: #7f0000">
	 <xsl:choose>
	  <xsl:when test="@source='trips'">ONT</xsl:when>
	  <xsl:otherwise>WN</xsl:otherwise>
	 </xsl:choose>
	 <xsl:text>::</xsl:text>
	 <xsl:value-of select="@onttype" />
	</a>
	(<a href="javascript:toggleVisible('{$pos}-{@onttype}-synset')" id="{$pos}-{@onttype}-synset-link">show synset</a>)
	<span style="display: none" id="{$pos}-{@onttype}-synset"><xsl:value-of select="@words" /></span>
	(<a href="javascript:toggleVisible('{$pos}-{@onttype}-ancestors')" id="{$pos}-{@onttype}-ancestors-link">show ancestors</a>)
	<span style="display: none" id="{$pos}-{@onttype}-ancestors"><xsl:value-of select="@ancestors" /></span>
       </dt>
       <dd class="{@source}">
        <xsl:if test="/WORDS/@allow-editing and @source!='trips'">
	 <form action="lex-ont-edit">
	  <input type="hidden" name="op" value="add-ont-type-for-wn-sense" />
	  <input type="hidden" name="arg" value="{@onttype}" />
	  <input type="submit" value="add to TRIPS" />
	 </form>
	</xsl:if>
	<xsl:if test="@gloss">
	 Gloss: <xsl:value-of select="@gloss" /><br />
	</xsl:if>
	Frames:
	<dl style="padding-left: 2.0em">
	 <xsl:for-each select="FRAME">
	  <dt style="color: #007f00"><xsl:value-of select="@desc" /></dt>
	  <!-- nilled is not supported by IE or Firefox, only Safari -->
	  <!-- xsl:if test="not(nilled(@example))" -->
	  <xsl:if test="@example">
	   <dd>Example: <i><xsl:value-of select="@example" /></i></dd>
	  </xsl:if>
	 </xsl:for-each>
	</dl>
       </dd>
      </xsl:for-each>
     </dl>
    </li>
   </xsl:for-each>
  </ul>
 </xsl:for-each>
 <p>Data file last modified: <xsl:value-of select="@modified" /></p>
 </body>
 </html>
</xsl:template>

</xsl:stylesheet>
