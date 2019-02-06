<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:exsl="http://exslt.org/common"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                xmlns="http://www.w3.org/1999/xhtml">
 <xsl:import href="parser-interface.xsl" />

 <xsl:template match="/trips-parser-output | /texttagger-output">
  <xsl:variable name="title">
   <xsl:text>TRIPS/DRUM</xsl:text>
   <xsl:if test="@service = 'DRUM-DEV'">-DEV</xsl:if>
   <xsl:if test="@extscontents">-ER</xsl:if>
   <xsl:text> </xsl:text>
   <xsl:choose>
    <xsl:when test="/trips-parser-output">
     <xsl:text>Parser</xsl:text>
    </xsl:when>
    <xsl:otherwise>
     <xsl:text>TextTagger</xsl:text>
    </xsl:otherwise>
   </xsl:choose>
   <xsl:text> Web Interface</xsl:text>
  </xsl:variable>
  <html>
   <head>
    <title><xsl:value-of select="$title" /></title>
    <xsl:call-template name="scripts-and-styles" />
   </head>
   <body onload="bodyLoaded()">
    <h1><xsl:value-of select="$title" /></h1>
    <xsl:variable name="query">
     <xsl:if test="/texttagger-output">
      <xsl:text>?component=texttagger</xsl:text>
     </xsl:if>
    </xsl:variable>
    <xsl:choose>
     <xsl:when test="@service = 'DRUM-DEV' or @service = 'DRUM-ER'">
      <p>(This is the nightly-updated version of the DRUM web service; see also the <a href="drum{$query}">stable 2017 version</a>.)</p>
     </xsl:when>
     <xsl:otherwise>
      <p>(This is the stable 2017 version of the DRUM web service; see also the <a href="drum-dev{$query}">nightly-updated version</a>.)</p>
     </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="@error">
     <p>Error: <xsl:value-of select="@error" /></p>
    </xsl:if>
    <form action="#" method="POST">
     <textarea name="input" placeholder="Enter a paragraph." cols="80" rows="20" maxlength="8000"><xsl:value-of select="@input" /></textarea><br />
     <input type="submit">
      <xsl:attribute name="value">
       <xsl:choose>
        <xsl:when test="/trips-parser-output">Parse</xsl:when>
	<xsl:otherwise>Tag</xsl:otherwise>
       </xsl:choose>
      </xsl:attribute>
     </input><br />
     <div><em>Interface options:</em>
      <xsl:call-template name="interface-options" />
     </div>
     <div><em>TextTagger options:</em><br/>
	<label>Input terms:<br/>
	 <textarea name="input-terms" id="input-terms" placeholder="Enter a Lispy list of native tags without :start/:end."><xsl:value-of select="@input-terms" /></textarea></label>
     </div>
    </form>
    <xsl:apply-templates />
    <xsl:call-template name="footer" />
   </body>
  </html>
 </xsl:template>
</xsl:stylesheet>

