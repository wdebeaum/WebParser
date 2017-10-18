<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:exsl="http://exslt.org/common"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="str exsl">
<!--
exts-to-table.xsl - convert extractions to an HTML table
William de Beaumont
2016-06-15
  -->

<xsl:output method="xml" encoding="UTF-8" />

<!-- don't copy text or attribute nodes by default -->
<xsl:template match="text()|@*" mode="exts-to-table" priority="-1" />

<xsl:template name="no-ont">
 <xsl:param name="str" />
 <xsl:choose>
  <xsl:when test="starts-with($str, 'ONT::')">
   <xsl:for-each select="exsl:node-set(substring($str, 6))">
    <xsl:call-template name="trips-ont-link" /><!-- from lf-to-html.xsl -->
   </xsl:for-each>
  </xsl:when>
  <xsl:otherwise>
   <xsl:value-of select="$str" />
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!-- wrapper for tags-to-table.xsl's drum-id-link that interprets |s -->
<xsl:template name="drum-id-link-2">
 <xsl:variable name="toks">
  <xsl:call-template name="str:tokenize">
   <xsl:with-param name="string" select="." />
   <xsl:with-param name="delimiters" select="'|'" />
  </xsl:call-template>
 </xsl:variable>
 <xsl:for-each select="exsl:node-set($toks)/token">
  <xsl:if test="position() != 1">
   <xsl:text> | </xsl:text>
  </xsl:if>
  <xsl:call-template name="drum-id-link" />
 </xsl:for-each>
</xsl:template>

<xsl:template match="type" mode="exts-to-table">
 <xsl:call-template name="no-ont">
  <xsl:with-param name="str" select="." />
 </xsl:call-template>
</xsl:template>

<xsl:template name="ext-common">
 <!-- ID -->
 <td><xsl:value-of select="@id" /></td>
 <!-- original text -->
 <td>
  <xsl:choose>
   <xsl:when test="string-length(text) &lt;=23">
    <xsl:value-of select="text" />
   </xsl:when>
   <xsl:otherwise>
    <xsl:value-of select="substring(text,1,10)" />
    <xsl:text>...</xsl:text>
    <xsl:value-of select="substring(text, string-length(text) - 9)" />
   </xsl:otherwise>
  </xsl:choose>
 </td>
 <!-- start/end -->
 <td><xsl:value-of select="@start" /></td>
 <td><xsl:value-of select="@end" /></td>
 <!-- ONT type -->
 <td><xsl:apply-templates select="type" mode="exts-to-table" /></td>
</xsl:template>

<xsl:template name="feature-ish">
 <xsl:if test="parent::not-features">
  <xsl:text>NOT </xsl:text>
 </xsl:if>
 <xsl:value-of select="local-name()" />
 <xsl:text>: </xsl:text>
 <xsl:apply-templates select="type" mode="exts-to-table" />
 <xsl:choose>
  <xsl:when test="value">
   <xsl:value-of select="value" />
  </xsl:when>
  <xsl:when test="text">
   <xsl:text> "</xsl:text>
   <xsl:value-of select="text" />
   <xsl:text>"</xsl:text>
  </xsl:when>
  <xsl:when test="text()">
   <xsl:text> "</xsl:text>
   <xsl:value-of select="." />
   <xsl:text>"</xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:apply-templates select="." mode="exts-to-table" />
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="EVENT | EPI | CC" mode="exts-to-table">
 <tr>
  <xsl:call-template name="ext-common" />
  <!-- predicate -->
  <!-- td><xsl:apply-templates select="predicate/type" mode="exts-to-table" /></td -->
  <!-- arguments -->
  <td>
   <xsl:for-each select="*[starts-with(local-name(), 'arg')]">
    <xsl:if test="position() != 1">
     <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:value-of select="substring(@role, 2)" />
    <xsl:text>: </xsl:text>
    <xsl:apply-templates select="type" mode="exts-to-table" />
    <xsl:text> </xsl:text>
    <xsl:value-of select="@id" />
    <xsl:text> "</xsl:text>
    <xsl:value-of select="text" />
    <xsl:text>"</xsl:text>
   </xsl:for-each>
  </td>
  <!-- other information -->
  <td>
   <xsl:if test="negation = '+'">
    <xsl:text>negated; </xsl:text>
   </xsl:if>
   <xsl:for-each select="mods/*">
    <xsl:call-template name="feature-ish" />
    <xsl:text>; </xsl:text>
   </xsl:for-each>
   <xsl:for-each select="features/inevent">
    <xsl:text>in event(s): </xsl:text>
    <xsl:apply-templates select="." mode="exts-to-table" />
    <xsl:text>; </xsl:text>
   </xsl:for-each>
   <xsl:for-each select="from-location">
    <xsl:text>from: </xsl:text>
    <xsl:apply-templates select="type" mode="exts-to-table" />
    <xsl:text> </xsl:text>
    <xsl:value-of select="@id" />
    <xsl:text> "</xsl:text>
    <xsl:value-of select="text" />
    <xsl:text>"; </xsl:text>
   </xsl:for-each>
   <xsl:for-each select="location">
    <xsl:text>at: </xsl:text>
    <xsl:apply-templates select="type" mode="exts-to-table" />
    <xsl:text> </xsl:text>
    <xsl:value-of select="@id" />
    <xsl:text> "</xsl:text>
    <xsl:value-of select="text" />
    <xsl:text>"; </xsl:text>
   </xsl:for-each>
   <xsl:for-each select="to-location">
    <xsl:text>to: </xsl:text>
    <xsl:apply-templates select="type" mode="exts-to-table" />
    <xsl:text> </xsl:text>
    <xsl:value-of select="@id" />
    <xsl:text> "</xsl:text>
    <xsl:value-of select="text" />
    <xsl:text>"; </xsl:text>
   </xsl:for-each>
   <xsl:for-each select="site | cell-line">
    <xsl:value-of select="local-name()" />
    <xsl:text>: </xsl:text>
    <xsl:apply-templates select="type" mode="exts-to-table" />
    <xsl:text> </xsl:text>
    <xsl:value-of select="@id" />
    <xsl:text> "</xsl:text>
    <xsl:value-of select="text" />
    <xsl:text>"; </xsl:text>
   </xsl:for-each>
  </td>
 </tr>
</xsl:template>

<xsl:template match="location" mode="exts-to-table">
 <xsl:value-of select="@id" />
</xsl:template>

<xsl:template match="mutation" mode="exts-to-table">
 <xsl:choose>
  <xsl:when test="@id">
   <xsl:value-of select="@id" />
  </xsl:when>
  <xsl:when test="./*">
   <xsl:text>(</xsl:text>
   <xsl:for-each select="./*">
    <xsl:if test="position() != 1">
     <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:value-of select="local-name()" />
    <xsl:text>: </xsl:text>
    <xsl:choose>
     <xsl:when test="text()">
      <xsl:value-of select="." />
     </xsl:when>
     <xsl:otherwise>
      <xsl:apply-templates select="." mode="exts-to-table" />
     </xsl:otherwise>
    </xsl:choose>
   </xsl:for-each>
   <xsl:text>)</xsl:text>
  </xsl:when>
  <xsl:otherwise> <!-- this happens when applied from exts-to-rdf.xsl -->
   <xsl:value-of select="." />
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="site|aa" mode="exts-to-table">
 <xsl:value-of select="name" />
 <xsl:if test="code">
  <xsl:text>(</xsl:text>
  <xsl:value-of select="code" />
  <xsl:text>)</xsl:text>
 </xsl:if>
 <xsl:if test="pos">
  <xsl:text>-</xsl:text>
  <xsl:value-of select="pos" />
 </xsl:if>
</xsl:template>

<xsl:template match="inevent" mode="exts-to-table">
 <xsl:for-each select="./@id">
  <xsl:if test="position() != 1">
   <xsl:text>, </xsl:text>
  </xsl:if>
  <xsl:value-of select="." />
 </xsl:for-each>
</xsl:template>

<!-- features -->
<xsl:template match="coref | assoc-with | ptm | bound-to | equals" mode="exts-to-table">
 <xsl:if test="@type">
  <xsl:text>(</xsl:text>
  <xsl:value-of select="@type" />
  <xsl:text>) </xsl:text>
 </xsl:if>
 <xsl:value-of select="@id" />
 <xsl:if test="@event"> <!-- for ptm|bound-to -->
  <xsl:text> (event: </xsl:text>
  <xsl:value-of select="@event" />
  <xsl:text>)</xsl:text>
 </xsl:if>
 <!-- NOTE: ignore experimental equals/@provenance -->
</xsl:template>

<xsl:template match="TERM" mode="exts-to-table">
 <tr>
  <xsl:call-template name="ext-common" />
  <!-- DB ID -->
  <td>
   <xsl:for-each select="@dbid">
    <xsl:call-template name="drum-id-link-2" />
   </xsl:for-each>
  </td>
  <!-- name -->
  <td><xsl:value-of select="name" /></td>
  <!-- features -->
  <td>
   <xsl:for-each select="not-features/* | features/* | mutation | mods/* | coref | assoc-with">
    <xsl:if test="position() != 1">
     <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:call-template name="feature-ish" />
   </xsl:for-each>
  </td>
  <!-- subterms -->
  <td>
   <xsl:if test="aggregate">
    <xsl:variable name="op" select="aggregate/@operator" />
    <xsl:for-each select="aggregate/member">
     <xsl:if test="position() != 1">
      <xsl:text> </xsl:text>
      <xsl:value-of select="$op" />
      <xsl:text> </xsl:text>
     </xsl:if>
     <xsl:value-of select="@id" /><!-- TODO look up ID? -->
    </xsl:for-each>
   </xsl:if>
   <xsl:if test="members">
    <xsl:if test="aggregate"><xsl:text>; </xsl:text></xsl:if>
    <xsl:text>members: </xsl:text>
    <xsl:for-each select="members/member">
     <xsl:if test="position() != 1"><xsl:text>, </xsl:text></xsl:if>
     <xsl:for-each select="@dbid">
      <xsl:call-template name="drum-id-link" />
     </xsl:for-each>
    </xsl:for-each>
   </xsl:if>
  </td>
 </tr>
</xsl:template>

<xsl:template name="event-like-headings">
 <xsl:param name="type" />
 <tr>
  <th title="The ID of this {$type}.">ID</th>
  <th title="The text as it appeared in the input.">text</th>
  <th title="The character offset of the start of the {$type} in the input.">start</th>
  <th title="The character offset of the end of the {$type} in the input.">end</th>
  <th title="The TRIPS ontology type of the {$type}.">ONT type</th>
  <!-- th title="">predicate</th -->
  <th title="">arguments</th>
  <th title="">other information</th>
 </tr>
</xsl:template>

<xsl:template match="ekb" mode="exts-to-table">
 <h3>Events</h3>
 <table class="tags"><!-- steal CSS from TT tags table -->
  <xsl:call-template name="event-like-headings">
   <xsl:with-param name="type" select="'event'" />
  </xsl:call-template>
  <xsl:apply-templates select="EVENT" mode="exts-to-table" />
 </table>
 <h3>Terms</h3>
 <table class="tags">
  <tr>
   <th title="The ID of this term.">ID</th>
   <th title="The text as it appeared in the input.">text</th>
   <th title="The character offset of the start of the term in the input.">start</th>
   <th title="The character offset of the end of the term in the input.">end</th>
   <th title="The TRIPS ontology type of the term.">ONT type</th>
   <th title="An ID from an external database corresponding to this term.">DB ID</th>
   <th title="">name</th>
   <th title="Features of this term.">features</th>
   <th title="IDs of terms that are part of this term.">subterms</th>
  </tr>
  <xsl:apply-templates select="TERM" mode="exts-to-table" />
 </table>
 <h3>Epistemic Modalities</h3>
 <table class="tags">
  <xsl:call-template name="event-like-headings">
   <xsl:with-param name="type" select="'modality'" />
  </xsl:call-template>
  <xsl:apply-templates select="EPI" mode="exts-to-table" />
 </table>
 <h3>Causal Connectives</h3>
 <table class="tags">
  <xsl:call-template name="event-like-headings">
   <xsl:with-param name="type" select="'connective'" />
  </xsl:call-template>
  <xsl:apply-templates select="CC" mode="exts-to-table" />
 </table>
</xsl:template>

</xsl:stylesheet>
