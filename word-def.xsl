<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exsl="http://exslt.org/common"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                xmlns="http://www.w3.org/1999/xhtml">

 <xsl:output method="xml" encoding="UTF-8" />
 
 <xsl:template match="feature-list-type">
  <xsl:apply-templates />
  <xsl:text>; </xsl:text>
 </xsl:template>

 <xsl:template match="var">
  <xsl:choose>
   <xsl:when test="option">
    <xsl:for-each select="option">
     <xsl:if test="position() != 1">
      <xsl:text> </xsl:text>
      <i>or</i>
      <xsl:text> </xsl:text>
     </xsl:if>
     <xsl:apply-templates />
    </xsl:for-each>
   </xsl:when>
   <xsl:otherwise>
    <xsl:text>?</xsl:text>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>

 <xsl:template match="feat">
  <xsl:if test="@name != 'type' and not(starts-with(., 'F::ANY-'))">
   <xsl:value-of select="@name" />
   <xsl:text>: </xsl:text>
   <xsl:apply-templates />
   <xsl:text>; </xsl:text>
  </xsl:if>
 </xsl:template>

 <xsl:template match="lex-entry">
  <dt>
   <xsl:value-of select="feat[@name='lf']/sense/@word" />
   <xsl:text> (</xsl:text>
   <xsl:value-of select="@pos" />
   <xsl:choose>
    <xsl:when test="feat[@name='vform']">
     <xsl:text>, </xsl:text>
     <xsl:apply-templates select="feat[@name='vform']" />
    </xsl:when>
    <xsl:when test="feat[@name='agr']">
     <xsl:text>, </xsl:text>
     <xsl:apply-templates select="feat[@name='agr'][1]" />
    </xsl:when>
   </xsl:choose>
   <xsl:text>)</xsl:text>
  </dt>
  <dd>
   <xsl:text>Ontology type: </xsl:text>
   <xsl:value-of select="feat[@name='lf']/sense/@ont-type" />
   <br />
   <xsl:text>Semantic features: </xsl:text>
   <xsl:apply-templates select="feat[@name='sem']/*" />
   <xsl:choose>
    <xsl:when test="feat[@name='template']">
     <br />
     <xsl:apply-templates select="feat[@name='template']" />
    </xsl:when>
    <xsl:when test="feat[@name='roles']/constit">
     <br />
     <xsl:text>Roles: </xsl:text>
     <xsl:for-each select="feat[@name='roles']/constit/feat">
      <xsl:if test="position() != 1">
       <xsl:text>, </xsl:text>
      </xsl:if>
      <span>
       <xsl:attribute name="title">
        <xsl:apply-templates select="." />
       </xsl:attribute>
       <xsl:value-of select="@name" />
      </span>
     </xsl:for-each>
    </xsl:when>
   </xsl:choose>
  </dd>
 </xsl:template>

 <xsl:template match="/word-def">
  <html>
  <head>
  <title>get word def</title>
  </head>
  <body>
  <h1>get word def</h1>
  <form action="#">
   <input type="text" name="w" placeholder="word" value="{@word}" />
   <input type="submit" value="get def" />
  </form>
  <dl>
   <xsl:apply-templates />
  </dl>
  </body>
  </html>
 </xsl:template>

</xsl:stylesheet>
