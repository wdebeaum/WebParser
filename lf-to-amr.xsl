<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:role="http://www.cs.rochester.edu/research/trips/role#"
    xmlns:TMA="http://www.cs.rochester.edu/research/trips/TMA#"
    xmlns:LF="http://www.cs.rochester.edu/research/trips/LF#">
<!--
lf-to-amr.xsl - convert an LF (in RDF format, as output by lf-to-rdf) to HTML containing pretty-printed AMR syntax
William de Beaumont
2015-03-04

Note: this relies on some templates in lf-to-html.xsl, but does not include it
directly because the main parser-interface.xsl already includes both.
  -->    
      
<output method="xml" encoding="UTF-8" />
                  
<!-- don't copy text or attribute nodes by default -->
<xsl:template match="text()|@*" mode="lf-to-amr" priority="-1" />
      
<!-- but do copy text nodes that aren't just whitespace, and assume they're ONT symbols unless they're lists -->
<xsl:template match="text()[normalize-space(.) != '']" mode="lf-to-amr">
 <xsl:apply-templates select="." mode="lf-to-html" />
</xsl:template>

<xsl:template match="@rdf:resource" mode="lf-to-amr">
 <xsl:param name="indent" select="''" />
 <xsl:param name="id-stack" select="''" />
 <xsl:variable name="frag" select="string(.)" />
 <xsl:variable name="id" select="substring(.,2)" />
 <xsl:choose>
  <xsl:when test="contains($id-stack, concat(' ', $id, ' ')) or ../../preceding-sibling::rdf:Description/role:*[@rdf:resource=$frag] or not(ancestor::rdf:RDF/rdf:Description[@rdf:ID=$id])">
   <!-- already seen this ID before (or it's from another utt),
     just use the ID itself -->
   <xsl:call-template name="id">
    <xsl:with-param name="id" select="$id" />
   </xsl:call-template>
  </xsl:when>
  <xsl:otherwise>
   <!-- this is the first use of this ID in its own utt,
     look up its description -->
   <xsl:apply-templates select="ancestor::rdf:RDF/rdf:Description[@rdf:ID=$id]" mode="lf-to-amr">
    <xsl:with-param name="indent" select="$indent" />
    <xsl:with-param name="id-stack" select="concat($id-stack, ' ', $id, ' ')" />
   </xsl:apply-templates>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="role:*" mode="lf-to-amr">
 <xsl:param name="indent" select="''" />
 <xsl:param name="id-stack" select="''" />
 <xsl:text>
</xsl:text>
 <xsl:value-of select="$indent" />
 <xsl:text>:</xsl:text>
 <xsl:value-of select="local-name()" />
 <xsl:text> </xsl:text>
 <xsl:apply-templates select="*|@*|text()" mode="lf-to-amr">
  <xsl:with-param name="indent" select="$indent" />
  <xsl:with-param name="id-stack" select="$id-stack" />
 </xsl:apply-templates>
</xsl:template>

<xsl:template match="rdf:Description" mode="lf-to-amr">
 <xsl:param name="indent" select="''" />
 <xsl:param name="id-stack" select="''" />
 <!-- ID -->
 <xsl:text>(</xsl:text>
 <xsl:call-template name="id">
  <xsl:with-param name="id" select="@rdf:ID" />
 </xsl:call-template>
 <xsl:text> / </xsl:text>

 <!-- type -->
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
   <xsl:apply-templates select="LF:type" mode="lf-to-html" />
  </xsl:otherwise>
 </xsl:choose>

 <!-- arguments -->
 <xsl:for-each select="role:*">
  <xsl:if test="not(local-name() = 'OF' and ../LF:type = 'SET')">
   <xsl:apply-templates select="." mode="lf-to-amr">
    <xsl:with-param name="indent" select="concat($indent,'               ')" />
    <xsl:with-param name="id-stack" select="concat($id-stack, ' ', ../@rdf:ID, ' ')" />
   </xsl:apply-templates>
  </xsl:if>
 </xsl:for-each>

 <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="rdf:RDF" mode="lf-to-amr">
<pre>
<xsl:apply-templates select="rdf:Description[1]" mode="lf-to-amr" />
</pre>
</xsl:template>

</xsl:stylesheet>
