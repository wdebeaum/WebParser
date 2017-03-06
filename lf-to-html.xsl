<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:role="http://www.cs.rochester.edu/research/trips/role#"
    xmlns:TMA="http://www.cs.rochester.edu/research/trips/TMA#"
    xmlns:LF="http://www.cs.rochester.edu/research/trips/LF#">
<!--
lf-to-html.xsl - convert an LF (in RDF format, as output by lf-to-rdf) to HTML containing a pretty-printed lisp form
William de Beaumont
2010-02-02
  -->    

<output method="xml" encoding="UTF-8" />

<!-- don't copy text or attribute nodes by default -->
<xsl:template match="text()|@*" mode="lf-to-html" priority="-1" />

<!-- but do copy text nodes that aren't just whitespace, and assume they're ONT symbols, unless they're lists -->
<xsl:template match="text()[normalize-space(.) != '']" mode="lf-to-html">
 <xsl:if test="not(starts-with(.,'('))">
  <xsl:text>ONT::</xsl:text>
 </xsl:if>
 <xsl:value-of select="." />
</xsl:template>

<xsl:template name="id">
 <xsl:param name="id" />
 <xsl:text>ONT::</xsl:text>
 <b class="term{count(//rdf:Description[@rdf:ID=$id]/preceding-sibling::rdf:Description)}">
 <xsl:value-of select="$id" />
 </b>
</xsl:template>

<xsl:template match="@rdf:resource" mode="lf-to-html">
 <xsl:call-template name="id">
  <xsl:with-param name="id" select="substring(.,2)" />
 </xsl:call-template>
</xsl:template>

<xsl:template match="@rdf:ID" mode="lf-to-html">
 <xsl:call-template name="id">
  <xsl:with-param name="id" select="." />
 </xsl:call-template>
</xsl:template>

<xsl:template match="LF:indicator" mode="lf-to-html">
 <xsl:text>ONT::</xsl:text><xsl:value-of select="." />
</xsl:template>

<xsl:template name="trips-ont-link">
 <a href="http://www.cs.rochester.edu/research/trips/lexicon/cgi/browseontology-ajax?search={.}#highlight">
 <xsl:text>ONT::</xsl:text><xsl:value-of select="." />
 </a>
</xsl:template>

<xsl:template match="LF:type" mode="lf-to-html">
 <xsl:call-template name="trips-ont-link" />
</xsl:template>

<xsl:template name="trips-lex-link">
 <a href="http://www.cs.rochester.edu/research/trips/lexicon/data/W::{.}.xml">
 <xsl:text>W::</xsl:text><xsl:value-of select="." />
 </a>
</xsl:template>

<xsl:template match="LF:word" mode="lf-to-html">
 <xsl:call-template name="trips-lex-link" />
</xsl:template>

<xsl:template match="role:*" mode="lf-to-html">
 <xsl:text> :</xsl:text><xsl:value-of select="local-name()" />
 <xsl:text> </xsl:text><xsl:apply-templates select="*|@*|text()" mode="lf-to-html" />
</xsl:template>

<xsl:template match="rdf:Description" mode="lf-to-html">
 <xsl:text>(</xsl:text><xsl:apply-templates select="LF:indicator" mode="lf-to-html" />
 <xsl:text> </xsl:text><xsl:apply-templates select="@rdf:ID" mode="lf-to-html" />

 <!-- type -->
 <xsl:text> </xsl:text>
 <xsl:choose>
  <xsl:when test="LF:type = 'SET'">
   <xsl:text>(ONT::SET-OF </xsl:text><xsl:apply-templates select="role:OF/@rdf:resource" mode="lf-to-html" />
   <xsl:text>)</xsl:text>
  </xsl:when>
  <xsl:when test="LF:word">
   <xsl:text>(:* </xsl:text><xsl:apply-templates select="LF:type" mode="lf-to-html" />
   <xsl:text> </xsl:text><xsl:apply-templates select="LF:word" mode="lf-to-html" />
   <xsl:text>)</xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:apply-templates select="LF:type" />
  </xsl:otherwise>
 </xsl:choose>
 
 <!-- normal roles -->
 <xsl:for-each select="role:*">
  <!-- xsl:if test="not((local-name() = 'OF' and ../LF:type = 'SET') or local-name() = 'MOD' or local-name() = 'MEMBER' or local-name() = 'TENSE' or local-name() = 'MODE' or local-name() = 'ASPECT')" -->
  <xsl:if test="not((local-name() = 'OF' and ../LF:type = 'SET') or local-name() = 'MOD' or local-name() = 'MEMBER')">
   <xsl:apply-templates select="." mode="lf-to-html" />
  </xsl:if>
 </xsl:for-each>

 <!-- mods -->
 <xsl:if test="role:MOD">
  <xsl:text> :MODS (</xsl:text>
  <xsl:for-each select="role:MOD">
   <xsl:if test="position() > 1"><xsl:text> </xsl:text></xsl:if>
   <xsl:apply-templates select="@rdf:resource" mode="lf-to-html" />
  </xsl:for-each>
  <xsl:text>)</xsl:text>
 </xsl:if>

 <!-- members -->
 <xsl:if test="role:MEMBER">
  <xsl:text> :MEMBERS (</xsl:text>
  <xsl:for-each select="role:MEMBER">
   <xsl:if test="position() > 1"><xsl:text> </xsl:text></xsl:if>
   <xsl:apply-templates select="@rdf:resource" mode="lf-to-html" />
  </xsl:for-each>
  <xsl:text>)</xsl:text>
 </xsl:if>

 <!-- TMA -->
 <!-- xsl:if test="role:TENSE|role:MODE|role:ASPECT">
  <xsl:text> :TMA (</xsl:text>
  <xsl:for-each select="role:TENSE|role:MODE|role:ASPECT">
   <xsl:if test="position() > 1"><xsl:text> </xsl:text></xsl:if>
   <xsl:text>(W::</xsl:text><xsl:value-of select="local-name()" />
   <xsl:text> W::</xsl:text><xsl:value-of select="." />
   <xsl:text>)</xsl:text>
  </xsl:for-each>
  <xsl:text>)</xsl:text>
 </xsl:if -->

 <xsl:text>)
 </xsl:text>
</xsl:template>

<xsl:template match="rdf:RDF" mode="lf-to-html">
<!-- html>
<head>
<title>LF terms</title>
<script type="text/javascript">
<![CDATA[
function downcaseHack() {
  var links = document.getElementsByTagName("a");
  for (var i = 0; i < links.length; i++) {
    links[i].href = links[i].href.replace(/::.*?\./, function(str) { return str.toLowerCase() });
  }
}
]]>
</script>
<style type="text/css">
<![CDATA[
.term0 { background-color: #00BBFF }
.term1 { background-color: #00FF00 }
.term2 { background-color: #FF0000 }
.term3 { background-color: #CB00CE }
.term4 { background-color: #2BDD00 }
.term5 { background-color: #0092CC }
.term6 { background-color: #95CC00 }
.term7 { background-color: #FFAA00 }
.term8 { background-color: #8700FF }
.term9 { background-color: #A5700D }
.term10 { background-color: #BB7474 }
.term11 { background-color: #75C440 }
.term12 { background-color: #6A8DB7 }
]]>
</style>
</head>
<body onload="downcaseHack()" -->
<pre>
<xsl:text>(</xsl:text>
<xsl:apply-templates mode="lf-to-html" />
<xsl:text>)</xsl:text>
</pre>
<!-- /body>
</html -->
</xsl:template>

</xsl:stylesheet>

