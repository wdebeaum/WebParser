<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.w3.org/1999/xhtml">
<!-- tree-to-LinGO.xsl - convert a simple tree structure into a LinGO-style HTML table tree
William de Beaumont
2010-02-03
  -->

<xsl:output method="xml" encoding="UTF-8" />

<xsl:template name="make-rest-of-rows">
 <xsl:param name="first-row" />
 <tr>
  <xsl:variable name="preceding-text" select="$first-row[1]/preceding::text()" />
  <xsl:if test="$preceding-text">
   <td colspan="{count($preceding-text) * 2}"></td>
  </xsl:if>
  <xsl:for-each select="$first-row"> <!-- for each item in this row -->
   <!-- make a td for the item -->
   <xsl:choose>
    <xsl:when test="self::text()">
     <td class="LinGOLeaf">
      <xsl:value-of select="." />
     </td>
    </xsl:when>
    <xsl:otherwise> <!-- an element -->
     <td class="LinGONode" colspan="{count(.//text()) * 2 - 1}">
      <xsl:value-of select="local-name()" />
     </td>
    </xsl:otherwise>
   </xsl:choose>
   <td>
    <xsl:attribute name="colspan">
     <!-- Find the number of text nodes between the current node and the next
	  one in the row. The colspan will be this number * 2 + 1. -->
     <xsl:variable name="current-depth" select="count(ancestor::*)" />
     <xsl:variable name="rest-of-row" select="following::node()[count(ancestor::*) = $current-depth]" />
     <xsl:variable name="next-item" select="$rest-of-row[1]" />
     <xsl:value-of select="(count(following::text()) - count($next-item/following::text() | $next-item/descendant-or-self::text())) * 2 + 1" />
    </xsl:attribute>
   </td>
  </xsl:for-each>
 </tr>
 <!-- If there are more rows, recurse on the next one! -->
 <xsl:variable name="next-row" select="$first-row/node()" />
 <xsl:if test="$next-row">
  <xsl:call-template name="make-rest-of-rows">
   <xsl:with-param name="first-row" select="$next-row" />
  </xsl:call-template>
 </xsl:if>
</xsl:template>

<xsl:template match="*" mode="tree-to-LinGO">
<!-- html>
<head>
<title>LinGO tree</title>
<style type="text/css">
<![CDATA[
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
]]>
</style>
</head>
<body -->
<table cellspacing="0">

<!-- make sure we have enough columns -->
<tr>
 <xsl:for-each select=".//text()">
  <td><xsl:text> </xsl:text></td>
  <td><xsl:text> </xsl:text></td>
 </xsl:for-each>
</tr>

<!-- make all the rows -->
<xsl:call-template name="make-rest-of-rows">
 <xsl:with-param name="first-row" select="." />
</xsl:call-template>

</table>
<!-- /body>
</html -->
</xsl:template>

</xsl:stylesheet>
