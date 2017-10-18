<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:exsl="http://exslt.org/common"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="str exsl">
<!--
tags-to-table.xsl - convert TextTagger tags to an HTML table
William de Beaumont
2015-03-05
  -->

<xsl:import href="str.tokenize.template.xsl" />
<!-- already get str:replace in lf-to-dot.xsl -->

<xsl:output method="xml" encoding="UTF-8" />

<!-- don't copy text or attribute nodes by default -->
<xsl:template match="text()|@*" mode="tags-to-table" priority="-1" />

<xsl:template name="wn-sk-link">
 <!-- construct URL-encoded version of complete sense key -->
 <xsl:variable name="sk">
  <xsl:value-of select="substring-before(., '%')" />
  <xsl:text>%25</xsl:text> <!-- % -->
  <xsl:call-template name="str:replace">
   <xsl:with-param name="string" select="substring-after(., '%')" />
   <xsl:with-param name="search" select="':'" />
   <xsl:with-param name="replace" select="'%3A'" />
  </xsl:call-template>
   <!-- a roundabout way of testing whether the sense key is missing the last
   two colons: instead test whether the % is in the last 8 characters -->
  <xsl:if test="contains(substring(., string-length(.) - 7), '%')">
   <xsl:text>%3A%3A</xsl:text> <!-- :: -->
  </xsl:if>
 </xsl:variable>
 <a href="http://trips.ihmc.us/WordNetWeb/get-word-xml.pl?lang=en&amp;search={$sk}">
  <xsl:value-of select="." />
 </a>
</xsl:template>

<xsl:template name="drum-id-link">
 <xsl:variable name="pkg" select="substring-before(., ':')" />
 <xsl:variable name="id" select="substring-after(., ':')" />
 <xsl:choose>
  <xsl:when test="$pkg = 'BTO'">
   <a title="Brenda Tissue Ontology" href="http://www.ebi.ac.uk/ontology-lookup/browse.do?ontName=BTO&amp;termId=BTO%3A{$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'CHEBI'">
   <a title="Chemical Entities of Biological Interest" href="http://www.ebi.ac.uk/chebi/advancedSearchFT.do?searchString=CHEBI%3A{$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'CO'">
   <a title="Cell Ontology" href="http://bioportal.bioontology.org/ontologies/CL?p=classes&amp;conceptid=CL%3A{$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'EFO'">
   <a title="Experimental Factor Ontology" href="http://www.ebi.ac.uk/efo/EFO_{$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'GO'">
   <a title="Gene Ontology" href="http://amigo.geneontology.org/amigo/term/GO%3A{$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'HGNC'">
   <a title="HUGO Gene Nomenclature Committee" href="http://www.genenames.org/cgi-bin/gene_symbol_report?hgnc_id={$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'NCIT'">
   <a title="National Cancer Institute Thesaurus" href="https://ncit.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&amp;ns=NCI_Thesaurus&amp;code={$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <!-- couldn't find a good site for PSI-MI/MOD -->
  <xsl:when test="$pkg = 'SO'">
   <a title="Sequence Ontology" href="http://www.sequenceontology.org/miso/release_2.4/term/SO%3A{$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <!-- couldn't find a good site for UO -->
  <xsl:when test="$pkg = 'UP' and starts-with($id, 'SL-')">
   <a title="UniProt" href="http://www.uniprot.org/locations/{$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'UP'">
   <a title="UniProt" href="http://www.uniprot.org/uniprot/{$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'XFAM'">
   <xsl:variable name="nodot">
    <xsl:choose>
     <xsl:when test="contains($id, '.')">
      <xsl:value-of select="substring-before($id, '.')" />
     </xsl:when>
     <xsl:otherwise>
      <xsl:value-of select="$id" />
     </xsl:otherwise>
    </xsl:choose>
   </xsl:variable>
   <a title="Protein Families" href="http://pfam.xfam.org/family/{$nodot}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <xsl:when test="$pkg = 'MESH'">
   <a title="Medical Subject Headings" href="http://www.nlm.nih.gov/cgi/mesh/2015/MB_cgi?field=uid&amp;term={$id}">
    <xsl:value-of select="." />
   </a>
  </xsl:when>
  <!-- couldn't find a good site for Cellosaurus or neXtProt's FAmily -->
  <xsl:otherwise>
   <xsl:value-of select="." />
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="@ont-types" mode="tags-to-table">
 <xsl:variable name="toks">
  <xsl:call-template name="str:tokenize">
   <xsl:with-param name="string" select="." />
   <xsl:with-param name="delimiters" select="','" />
  </xsl:call-template>
 </xsl:variable>
 <xsl:for-each select="exsl:node-set($toks)/token">
  <xsl:if test="position() != 1">
   <xsl:text>, </xsl:text>
  </xsl:if>
  <xsl:call-template name="trips-ont-link" />
 </xsl:for-each>
</xsl:template>

<xsl:template match="pitch" mode="tags-to-table">
 <xsl:value-of select="@letter | @scale-degree" />
 <xsl:if test="@scale-degree">&#x302;</xsl:if> <!-- hat -->
 <xsl:choose>
  <xsl:when test="@semitones-above-natural &lt; 0">
   <xsl:value-of select="substring('♭♭♭', 1, - @semitones-above-natural)" />
  </xsl:when>
  <xsl:when test="@semitones-above-natural &gt; 0">
   <xsl:value-of select="substring('♯♯♯', 1, @semitones-above-natural)" />
  </xsl:when>
 </xsl:choose>
 <xsl:value-of select="@octave" />
</xsl:template>

<xsl:template match="interval" mode="tags-to-table">
 <xsl:value-of select="@quality" />
 <xsl:text> </xsl:text>
 <xsl:value-of select="@scale-degree-span" />
 <xsl:choose>
  <xsl:when test="@scale-degree-span = 2">nd</xsl:when>
  <xsl:when test="@scale-degree-span = 3">rd</xsl:when>
  <xsl:otherwise>th</xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="chord" mode="tags-to-table">
 <xsl:for-each select="@*|*">
  <xsl:value-of select="local-name()" />
  <xsl:text>: </xsl:text>
  <xsl:choose>
   <xsl:when test="self::node()">
    <xsl:apply-templates select="." mode="tags-to-table" />
   </xsl:when>
   <xsl:otherwise>
    <xsl:value-of select="." />
   </xsl:otherwise>
  </xsl:choose>
  <xsl:text>; </xsl:text>
 </xsl:for-each>
</xsl:template>

<xsl:template match="sense-info" mode="tags-to-table">
 <td><xsl:value-of select="@penn-parts-of-speech" /></td>
 <td>
  <xsl:apply-templates select="@ont-types" mode="tags-to-table" />
  <xsl:if test="@wn-sense-keys">
   <xsl:variable name="toks">
    <xsl:call-template name="str:tokenize">
     <xsl:with-param name="string" select="@wn-sense-keys" />
     <xsl:with-param name="delimiters" select="','" />
    </xsl:call-template>
   </xsl:variable>
   <xsl:for-each select="exsl:node-set($toks)/token">
    <xsl:if test="position() != 1">
     <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:call-template name="wn-sk-link" />
   </xsl:for-each>
  </xsl:if>
 </td>
 <td>
  <dl>
   <xsl:if test="@score">
    <dt class="short">score</dt>
    <dd><xsl:value-of select="@score" /></dd>
   </xsl:if>
   <xsl:if test="@alternate-spellings">
    <dt class="short">alternate spellings</dt>
    <dd><xsl:value-of select="@alternate-spellings" /></dd>
   </xsl:if>
   <xsl:if test="*">
    <xsl:for-each select="*">
     <dt><xsl:value-of select="local-name()" /></dt>
     <dd>
      <dl>
       <xsl:for-each select="@*|*">
	<dt class="short"><xsl:value-of select="local-name()" /></dt>
	<dd>
	 <xsl:choose>
	  <xsl:when test="self::map"><!-- maps are weird -->
	   <xsl:text>to </xsl:text>
	   <xsl:for-each select="@to">
	    <xsl:call-template name="trips-ont-link" />
	   </xsl:for-each>
	   <xsl:text> through </xsl:text>
	   <xsl:for-each select="@through">
	    <xsl:call-template name="drum-id-link" />
	   </xsl:for-each>
	  </xsl:when>
	  <xsl:when test="self::match"><!-- so are matches -->
	   <xsl:value-of select="@matched" />
	   <xsl:text> (score = </xsl:text>
	   <xsl:value-of select="@score" />
	   <xsl:text>)</xsl:text>
	  </xsl:when>
	  <!-- so are specialist links -->
	  <xsl:when test="self::nominalization | self::nominalization-of | self::abbreviation | self::abbreviation-of | self::acronym | self::acronym-of">
	   <xsl:for-each select="specialist">
	    <xsl:if test="position() != 1">
	     <xsl:text>, </xsl:text>
	    </xsl:if>
	    <xsl:value-of select="@eui" />
	    <xsl:text> (</xsl:text>
	    <xsl:value-of select="@citation-form" />
	    <xsl:if test="@cat">
	     <xsl:text> / </xsl:text>
	     <xsl:value-of select="@cat" />
	    </xsl:if>
	    <xsl:text>)</xsl:text>
	   </xsl:for-each>
	  </xsl:when>
	  <xsl:when test="amino-acid">
	   <xsl:for-each select="amino-acid">
	    <xsl:if test="position() != 1">
	     <xsl:text>, </xsl:text>
	    </xsl:if>
	    <xsl:value-of select="@name" />
	    <xsl:text> (</xsl:text>
	    <xsl:value-of select="@letter" />
	    <xsl:text>)</xsl:text>
	   </xsl:for-each>
	  </xsl:when>
	  <xsl:when test="aa-site">
	   <xsl:if test="aa-site/@name">
	    <xsl:value-of select="aa-site/@name" />
	    <xsl:text> (</xsl:text>
	    <xsl:value-of select="aa-site/@letter" />
	    <xsl:text>) at </xsl:text>
	   </xsl:if>
	   <xsl:value-of select="aa-site/@index" />
	  </xsl:when>
	  <xsl:when test="self::root | self::bass">
	   <xsl:apply-templates select="pitch" mode="tags-to-table" />
	  </xsl:when>
	  <xsl:when test="self::members">
	   <xsl:text>(see above)</xsl:text>
	   <!-- kind of a cop-out since I can't get the following to look right-->
	   <!-- xsl:for-each select="*">
	    <xsl:if test="position() != 1"><xsl:text>, </xsl:text></xsl:if>
	    <xsl:text>(</xsl:text>
	    <xsl:apply-templates select="." mode="tags-to-table" />
	    <xsl:text>)</xsl:text>
	   </xsl:for-each -->
	  </xsl:when>
	  <xsl:when test="self::intervals-above-bass | self::intervals-above-root">
	   <xsl:for-each select="*">
	    <xsl:if test="position() != 1"><xsl:text>, </xsl:text></xsl:if>
	    <xsl:apply-templates select="." mode="tags-to-table" />
	   </xsl:for-each>
	  </xsl:when>
	  <xsl:when test="local-name() = 'ont-types'"> <!-- ONT types aren't drum IDs -->
	   <xsl:apply-templates select="." mode="tags-to-table" />
	  </xsl:when>
	  <xsl:when test="contains(., ',') and contains(., ':')">
	   <xsl:variable name="toks">
	    <xsl:call-template name="str:tokenize">
	     <xsl:with-param name="string" select="." />
	     <xsl:with-param name="delimiters" select="','" />
	    </xsl:call-template>
	   </xsl:variable>
	   <xsl:for-each select="exsl:node-set($toks)/token">
	    <xsl:if test="position() != 1">
	     <xsl:text>, </xsl:text>
	    </xsl:if>
	    <xsl:call-template name="drum-id-link" />
	   </xsl:for-each>
	  </xsl:when>
	  <xsl:when test="contains(., ':')">
	   <xsl:call-template name="drum-id-link" />
	  </xsl:when>
	  <xsl:otherwise>
	   <xsl:value-of select="." />
	  </xsl:otherwise>
	 </xsl:choose>
	</dd>
       </xsl:for-each>
      </dl>
     </dd>
    </xsl:for-each>
   </xsl:if>
  </dl>
 </td>
</xsl:template>

<xsl:template match="tags" mode="tags-to-table">
 <table class="tags">
  <tr>
   <th title="The word or punctuation as it appears in the input.">word</th>
   <th title="The character offset of the start of the word in the input.">start</th>
   <th title="The character offset of the end of the word (really the start of the next word) in the input.">end</th>
   <th title="Parts of speech.">POS</th>
   <th title="TRIPS Ontology types or WordNet sense keys, linked to relevant browsers.">sense</th>
   <th title="Other information related to the specific word sense, with links to relevant ontology browsers.">more sense info</th>
  </tr>
  <xsl:for-each select="word|prefix">
   <tr>
    <xsl:variable name="rs">
     <xsl:choose>
      <xsl:when test="sense-info">
       <xsl:value-of select="count(sense-info)" />
      </xsl:when>
      <xsl:otherwise>1</xsl:otherwise>
     </xsl:choose>
    </xsl:variable>
    <td rowspan="{$rs}"><xsl:value-of select="@lex" /></td>
    <td rowspan="{$rs}"><xsl:value-of select="@start" /></td>
    <td rowspan="{$rs}"><xsl:value-of select="@end" /></td>
    <xsl:choose>
     <xsl:when test="sense-info">
      <xsl:apply-templates select="sense-info[1]" mode="tags-to-table" />
     </xsl:when>
     <xsl:otherwise>
      <td colspan="3"> </td>
     </xsl:otherwise>
    </xsl:choose>
   </tr>
   <xsl:for-each select="sense-info[position() != 1]">
    <tr>
     <xsl:apply-templates select="." mode="tags-to-table" />
    </tr>
   </xsl:for-each>
  </xsl:for-each>
 </table>
 <table class="tags">
  <tr>
   <th title="The phrase as it appears in the input.">phrase</th>
   <th title="The character offset of the start of the phrase in the input.">start</th>
   <th title="The character offset of the end of the phrase (really the start of the next word) in the input.">end</th>
   <th title="The syntactic categories of the phrase.">cat</th>
  </tr>
  <xsl:for-each select="prefer">
   <tr>
    <td>
     <xsl:choose>
      <xsl:when test="string-length(@text) &lt;= 23">
       <xsl:value-of select="@text" />
      </xsl:when>
      <xsl:otherwise>
       <xsl:value-of select="substring(@text,1,10)" />
       <xsl:text>...</xsl:text>
       <xsl:value-of select="substring(@text, string-length(@text) - 9)" />
      </xsl:otherwise>
     </xsl:choose>
    </td>
    <td><xsl:value-of select="@start" /></td>
    <td><xsl:value-of select="@end" /></td>
    <td><xsl:value-of select="@penn-cats" /></td>
   </tr>
  </xsl:for-each>
 </table>
</xsl:template>

</xsl:stylesheet>
