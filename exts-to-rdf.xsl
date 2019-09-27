<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:exsl="http://exslt.org/common"
    xmlns:str="http://exslt.org/strings"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:role="http://www.cs.rochester.edu/research/trips/role#"
    xmlns:LF="http://www.cs.rochester.edu/research/trips/LF#"
    extension-element-prefixes="str exsl">
<!--
exts-to-rdf.xsl - convert extractions to RDF (so that it can then be turned into a Graphviz graph by lf-to-rdf.xsl)
William de Beaumont
2016-06-15
(with contributions by Lucian Galescu)
  -->

<xsl:output method="xml" encoding="UTF-8" />

<!-- don't output any text or attributes unless I tell you to, and don't bother
     to process the input subtree at all -->
<xsl:template match="text()|@*|input" mode="exts-to-rdf" />

<!-- TT-derived features that we can borrow templates from exts-to-table for -->
<xsl:template match="mutation | site | aa" mode="exts-to-rdf">
 <xsl:choose>
  <xsl:when test="@id"> <!-- reference -->
   <xsl:element name="role:{local-name()}">
    <xsl:attribute name="rdf:resource">
     <xsl:text>#</xsl:text><xsl:value-of select="@id" />
    </xsl:attribute>
   </xsl:element>
  </xsl:when>
  <xsl:when test="parent::features"> <!-- literal in features -->
   <xsl:element name="role:{local-name()}">
    <xsl:apply-templates select="." mode="exts-to-table" />
   </xsl:element>
  </xsl:when>
  <xsl:otherwise> <!-- literal outside of features -->
   <LF:word><xsl:apply-templates select="." mode="exts-to-table" /></LF:word>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!-- database IDs, split on | -->
<xsl:template match="@dbid" mode="exts-to-rdf">
 <xsl:variable name="toks">
  <xsl:call-template name="str:tokenize">
   <xsl:with-param name="string" select="." />
   <xsl:with-param name="delimiters" select="'|'" />
  </xsl:call-template>
 </xsl:variable>
 <xsl:for-each select="exsl:node-set($toks)/token">
  <role:dbid><xsl:value-of select="." /></role:dbid>
 </xsl:for-each>
</xsl:template>

<!-- get rid of the ONT:: prefix if present -->
<xsl:template name="strip-ont-prefix">
 <xsl:choose>
  <xsl:when test="starts-with(., 'ONT::')">
   <xsl:value-of select="substring(., 6)" />
  </xsl:when>
  <xsl:otherwise>
   <xsl:value-of select="." />
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!-- things that turn into the LF:type of an RDF graph node -->
<xsl:template match="type | @type" mode="exts-to-rdf">
 <LF:type>
  <xsl:call-template name="strip-ont-prefix" />
 </LF:type>
</xsl:template>

<!-- (quoted) text -->
<xsl:template match="text[parent::TERM]" mode="exts-to-rdf">
 <role:text><xsl:text>"</xsl:text><xsl:value-of select="." /><xsl:text>"</xsl:text></role:text>
</xsl:template>

<!-- inevent role -->
<xsl:template match="inevent[@id]" mode="exts-to-rdf">
  <role:inevent rdf:resource="#{@id}" />
</xsl:template>

<!-- simple leaf values -->
<xsl:template match="tense | active | negation | polarity | modality | spec |
		     unit | min | max | amount[not(@id)] | value[not(@id)]"
	      mode="exts-to-rdf">
 <xsl:element name="role:{local-name()}">
  <xsl:value-of select="." />
 </xsl:element>
</xsl:template>

<!-- not sure this rule is actually necessary; this might happen anyway -->
<xsl:template match="features | not-features" mode="exts-to-rdf">
 <xsl:apply-templates mode="exts-to-rdf" />
</xsl:template>

<!-- turn aggregate into a single edge pointing to a single node, with member
     and except edges pointing out of it -->
<xsl:template match="aggregate" mode="exts-to-rdf">
 <role:aggregate>
  <rdf:Description rdf:ID="{generate-id()}">
   <LF:indicator>aggregate</LF:indicator>
   <LF:type><xsl:value-of select="@operator" /></LF:type>
   <xsl:for-each select="member">
    <role:member rdf:resource="#{@id}" />
   </xsl:for-each>
   <xsl:for-each select="except">
    <role:except rdf:resource="#{@id}" />
   </xsl:for-each>
  </rdf:Description>
 </role:aggregate>
</xsl:template>

<!-- split components into separate component roles, pointing to internal nodes
     in the RDF graph -->
<xsl:template match="components" mode="exts-to-rdf">
 <xsl:for-each select="component">
  <role:component rdf:resource="#{@id}" />
 </xsl:for-each>
</xsl:template>

<!-- turn the contents of . into a leaf node in the RDF graph, with a role edge
     going to it named based on the name of . -->
<xsl:template name="rdf-leaf-node">
 <xsl:variable name="role-name">
  <xsl:if test="parent::not-features"><xsl:text>not-</xsl:text></xsl:if>
  <xsl:value-of select="local-name()" />
 </xsl:variable>
 <xsl:element name="role:{$role-name}">
  <xsl:choose>
   <xsl:when test="@id"> <!-- reference -->
    <xsl:attribute name="rdf:resource">
     <xsl:text>#</xsl:text><xsl:value-of select="@id" />
    </xsl:attribute>
   </xsl:when>
   <!-- don't make full nodes for these, just use type|@type -->
   <xsl:when test="self::ptm | self::coref | self::scale">
    <xsl:for-each select="type | @type">
     <xsl:call-template name="strip-ont-prefix" />
    </xsl:for-each>
   </xsl:when>
   <xsl:otherwise> <!-- literal, full node -->
    <rdf:Description rdf:ID="{generate-id()}">
     <!-- FIXME degree|frequency|mods get redundant role/indicator -->
     <LF:indicator><xsl:value-of select="$role-name" /></LF:indicator>
     <xsl:apply-templates select="type | @type" mode="exts-to-rdf" />
     <xsl:if test="name | value">
      <LF:word><xsl:value-of select="name | value" /></LF:word>
     </xsl:if>
     <!-- TODO handle self::degree/text ? -->
    </rdf:Description>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:element>
</xsl:template>

<!-- groups of things that turn into leaf nodes in the RDF graph -->
<xsl:template match="mods | qualifiers" mode="exts-to-rdf">
 <xsl:for-each select="*">
  <xsl:call-template name="rdf-leaf-node" />
 </xsl:for-each>
</xsl:template>

<!-- modifier value -->
<xsl:template match="value[not(@id) and not(*)]" mode="exts-to-rdf">
  <xsl:element name="role:{local-name()}">
    <xsl:value-of select="." />
  </xsl:element>
</xsl:template>

<!-- timex and values expressions can be bushy, so we take a shortcut -->
<xsl:template match="timex | values" mode="exts-to-rdf">
  <xsl:element name="role:{local-name()}">
    <xsl:value-of select="." />
  </xsl:element>
</xsl:template>

<!-- xsl:template match="predicate" mode="exts-to-rdf">
 predicate no longer has any of these things
 <xsl:apply-templates select="negation | polarity | mods" mode="exts-to-rdf" />
</xsl:template -->

<!-- special case for meta-role mod on various roles -->
<xsl:template match="*[@mod]" mode="exts-to-rdf" priority="1">
 <!-- add rdf:ID so we can refer back to this edge when adding the mod later
 -->
 <xsl:element name="role:{local-name()}">
  <xsl:attribute name="rdf:ID">
   <xsl:value-of select="generate-id()" />
  </xsl:attribute>
  <xsl:choose>
   <xsl:when test="@id">
    <xsl:attribute name="rdf:resource">
     <xsl:text>#</xsl:text><xsl:value-of select="@id" />
    </xsl:attribute>
   </xsl:when>
   <xsl:otherwise>
    <xsl:value-of select="." />
   </xsl:otherwise>
  </xsl:choose>
 </xsl:element>
</xsl:template>

<!-- things that turn into leaf nodes in the RDF graph -->
<xsl:template match="cell-line | epistemic-modality | location | from-location
		     | to-location | coref | assoc-with | ptm | bound-to
		     | equals | size | scale | poss-by | quantifier | quantity
		     | over-quantity | time | refset | from-time | to-time"
	      mode="exts-to-rdf">
 <!-- NOTE: most of these always have @id, so they always take the first branch of rdf-leaf-node's choose. The exceptions are ptm and coref, which can take the second one. -->
 <!-- NOTE: ignoring (ptm|bound-to)/@event to make graph less busy -->
 <xsl:call-template name="rdf-leaf-node" />
</xsl:template>

<!-- arg and argN roles -->
<xsl:template match="*[starts-with(local-name(), 'arg')]" mode="exts-to-rdf">
 <xsl:element name="role:{substring(@role,2)}">
  <xsl:choose>
   <xsl:when test="@id">
    <xsl:attribute name="rdf:resource">
     <xsl:text>#</xsl:text><xsl:value-of select="@id" />
    </xsl:attribute>
   </xsl:when>
   <xsl:otherwise>
    <xsl:for-each select="type">
     <xsl:call-template name="strip-ont-prefix" />
    </xsl:for-each>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:element>
</xsl:template>

<!-- root elements -->
<xsl:template match="TERM | EVENT | MODALITY | EPI | CC" mode="exts-to-rdf">
 <!-- xsl:variable name="rdfid">
  <xsl:choose>
   <xsl:when test="self::CC">
    <xsl:text>CC-</xsl:text><xsl:value-of select="@id" />
   </xsl:when>
   <xsl:otherwise>
    <xsl:value-of select="@id" />
   </xsl:otherwise>
  </xsl:choose>
 </xsl:variable>
 <rdf:Description rdf:ID="{$rdfid}" -->
 <rdf:Description rdf:ID="{@id}">
  <LF:indicator><xsl:value-of select="local-name()" /></LF:indicator>
  <xsl:if test="name">
   <LF:word><xsl:value-of select="name" /></LF:word>
  </xsl:if>
  <!-- not sure why this isn't covered by the generic apply-templates, oh well -->
  <!-- xsl:apply-templates select="@dbid" mode="exts-to-rdf" / -->
  <xsl:apply-templates mode="exts-to-rdf" />
 </rdf:Description>
 <xsl:text>
</xsl:text>
</xsl:template>

<!-- top-level <ekb> element -->
<xsl:template match="ekb" mode="exts-to-rdf">
 <rdf:RDF>
  <xsl:apply-templates mode="exts-to-rdf" />
  <!-- special case for meta-role mod on various roles -->
  <xsl:for-each select=".//*[@mod]">
   <rdf:Description rdf:about="#{generate-id()}">
    <role:mod><xsl:value-of select="@mod" /></role:mod>
   </rdf:Description>
  </xsl:for-each>
 </rdf:RDF>
</xsl:template>

</xsl:stylesheet>
