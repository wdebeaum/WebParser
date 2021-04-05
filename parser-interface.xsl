<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:exsl="http://exslt.org/common"
		xmlns:str="http://exslt.org/strings"
		xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
		xmlns="http://www.w3.org/1999/xhtml">

 <xsl:output method="xml" encoding="UTF-8" />

 <xsl:include href="tree-to-lisp.xsl" />
 <xsl:include href="tree-to-LinGO.xsl" />
 <xsl:include href="tree-to-dot.xsl" />
 <xsl:include href="lf-to-html.xsl" />
 <xsl:include href="lf-to-amr.xsl" />
 <xsl:include href="lf-to-dot.xsl" />
 <xsl:include href="tags-to-table.xsl" />
 <xsl:include href="exts-to-table.xsl" />
 <xsl:include href="exts-to-rdf.xsl" />

 <xsl:param name="mode" />

 <xsl:template match="debug">
  <div id="debug-div" style="display: none">
   <h2 title="Output written to the Lisp console.">Debug Output</h2>
   <pre>
    <xsl:choose>
     <xsl:when test="/trips-parser-output/@trace-level=3">
      <xsl:value-of select="concat('==========================&#10;STARTING ITERATIVE EXTRACTIONS', substring-before(substring-after(., '==========================&#10;STARTING ITERATIVE EXTRACTIONS'), 'IM:      &#10;Processing SA (UTTERANCE-END '))" />
     </xsl:when>
     <xsl:otherwise><xsl:value-of select="." /></xsl:otherwise>
    </xsl:choose>
   </pre>
  </div>
 </xsl:template>

 <xsl:template match="ekb" mode="exts-to-lisp">
  <xsl:param name="type" />
  <pre class="exts-{$type}lisp">
   <xsl:text>(</xsl:text>
   <xsl:for-each select=".//@lisp">
    <xsl:value-of select="." />
    <xsl:text>
 </xsl:text>
   </xsl:for-each>
   <xsl:text>)
</xsl:text>
  </pre>
 </xsl:template>

 <xsl:template match="ekb">
  <xsl:variable name="type">
   <xsl:choose>
    <xsl:when test="count(../ekb) = 1"></xsl:when>
    <xsl:when test="count(preceding::ekb) = 0"><xsl:text>raw-</xsl:text></xsl:when>
    <xsl:when test="count(preceding::ekb) = 1"><xsl:text>inf-</xsl:text></xsl:when>
   </xsl:choose>
  </xsl:variable>
  <xsl:if test="count(preceding::ekb) = 0">
   <h2 title="Extracted terms, events, etc.">Extractions</h2>
  </xsl:if>
  <xsl:apply-templates select="." mode="exts-to-lisp">
   <xsl:with-param name="type" select="$type" />
  </xsl:apply-templates>
  <div class="exts-{$type}table">
   <xsl:apply-templates select="." mode="exts-to-table" />
  </div>
  <div class="exts-{$type}svg">
   <xsl:variable name="rdf">
    <xsl:apply-templates select="." mode="exts-to-rdf" />
   </xsl:variable>
   <xsl:apply-templates select="exsl:node-set($rdf)" mode="lf-to-dot" />
  </div>
 </xsl:template>

 <xsl:template match="word">
  <xsl:if test="position() > 1"><xsl:text> </xsl:text></xsl:if>
  <a>
   <xsl:choose>
    <xsl:when test="/trips-parser-output/@system = 'GLOSS'">
     <xsl:attribute name="href">lex-ont?side=lex&amp;q=<xsl:value-of select="." /></xsl:attribute>
     <xsl:attribute name="target">lexicon</xsl:attribute>
    </xsl:when>
    <xsl:otherwise>
     <xsl:attribute name="href">http://www.cs.rochester.edu/research/trips/lexicon/data/W::<xsl:value-of select="." />.xml</xsl:attribute>
    </xsl:otherwise>
   </xsl:choose>
   <xsl:value-of select="." />
  </a>
 </xsl:template>

 <xsl:template match="words">
  <h2 title="The list of words, as processed by the main TRIPS Parser module, linked to the TRIPS Lexicon Browser.">Words</h2>
  <p class="words"><xsl:apply-templates select="word" /></p>
 </xsl:template>

 <xsl:template match="tags">
  <h2 title="The messages sent from the TextTagger preprocessing module to the main Parser module.">Tags</h2>
  <pre class="tags-lisp">
   <xsl:value-of select="lisp" />
  </pre>
  <div class="tags-table">
   <xsl:apply-templates select="." mode="tags-to-table" />
  </div>
 </xsl:template>

 <xsl:template match="text()" mode="phrase">
  <xsl:text> </xsl:text>
  <xsl:value-of select="." />
 </xsl:template>

 <!-- non-phrase nodes -->
 <xsl:template match="*" mode="phrase" priority="-1">
  <xsl:apply-templates mode="phrase" />
 </xsl:template>

 <!-- phrase nodes -->
 <xsl:template match="UTT|NP|VP|ADJP|ADVBL|PP|CP" mode="phrase">
  <xsl:element name="{name()}">
   <xsl:apply-templates mode="phrase" />
  </xsl:element>
 </xsl:template>

 <xsl:template match="tree">
  <h2 title="The parse tree chosen as the best option by the parser.">Tree</h2>
  <pre class="tree-full-lisp">
   <xsl:value-of select="lisp" />
  </pre>
  <div class="tree-full-LinGO">
   <xsl:apply-templates select="*[2]" mode="tree-to-LinGO" />
  </div>
  <div class="tree-full-svg">
   <xsl:apply-templates select="*[2]" mode="tree-to-dot" />
  </div>
  <xsl:variable name="tree-phrase-xml">
   <xsl:apply-templates select="*[2]" mode="phrase" />
  </xsl:variable>
  <pre class="tree-phrase-lisp">
   <xsl:apply-templates select="exsl:node-set($tree-phrase-xml)" mode="tree-to-lisp" />
  </pre>
  <div class="tree-phrase-LinGO">
   <xsl:apply-templates select="exsl:node-set($tree-phrase-xml)" mode="tree-to-LinGO" />
  </div>
  <div class="tree-phrase-svg">
   <xsl:apply-templates select="exsl:node-set($tree-phrase-xml)" mode="tree-to-dot" />
  </div>
 </xsl:template>

 <xsl:template match="terms">
  <h2 class="lf" title="The Logical Form as described in the &quot;LF Documentation&quot; link below. Words and ONT types (in the Lisp and AMR formats) link to the TRIPS Lexicon and Ontology Browsers, respectively.">Logical Form</h2>
  <div class="lf-lisp">
   <xsl:apply-templates select="rdf:RDF" mode="lf-to-html" />
  </div>
  <div class="lf-amr">
   <xsl:apply-templates select="rdf:RDF" mode="lf-to-amr" />
  </div>
  <div class="lf-svg">
   <xsl:apply-templates select="rdf:RDF" mode="lf-to-dot" />
  </div>
 </xsl:template>

 <xsl:template match="alt-hyps">
  <xsl:for-each select="*">
   <div class="hyp-{position()}">
    <xsl:apply-templates select="." />
   </div>
  </xsl:for-each>
 </xsl:template>

 <!-- utterances according to the Parser -->
 <xsl:template match="utt">
  <xsl:choose>
   <xsl:when test="alt-hyps">
    <div class="hyp-0">
     <hr />
     <xsl:apply-templates select="words" />
     <xsl:apply-templates select="tags" />
     <xsl:apply-templates select="terms" />
     <xsl:apply-templates select="tree" />
    </div>
    <xsl:apply-templates select="alt-hyps" />
   </xsl:when>
   <xsl:otherwise>
    <hr />
    <xsl:apply-templates select="words" />
    <xsl:apply-templates select="tags" />
    <xsl:apply-templates select="terms" />
    <xsl:apply-templates select="tree" />
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>

 <xsl:template match="compound-communication-act[alt-hyps]">
  <div class="hyp-0">
   <xsl:apply-templates select="utt" />
  </div>
  <xsl:apply-templates select="alt-hyps" />
 </xsl:template>

 <!-- utterances according to TextTagger -->
 <xsl:template match="utterance">
  <hr />
  <h2 title="The text of this utterance.">Text</h2>
  <p><xsl:value-of select="@text" /></p>
  <xsl:apply-templates select="tags" />
 </xsl:template>

 <xsl:template match="failed-to-parse">
  <hr />
  <h2>Failed to parse</h2>
 </xsl:template>

 <xsl:template name="scripts-and-styles">
  <script type="text/javascript" src="../style/parser-interface.js"></script>
  <style type="text/css">
   <![CDATA[
    .exts-lisp { }
    .exts-table { }
    .exts-svg { }
    .exts-raw-lisp { }
    .exts-raw-table { }
    .exts-raw-svg { }
    .exts-inf-lisp { }
    .exts-inf-table { }
    .exts-inf-svg { }
    .tags-lisp { }
    .tags-table { }
    .tree-full-lisp { }
    .tree-full-LinGO { }
    .tree-full-svg { }
    .tree-phrase-lisp { }
    .tree-phrase-LinGO { }
    .tree-phrase-svg { }
    .lf-lisp { }
    .lf-amr { }
    .lf-svg { }
    #interface-options { display: none; }
    #texttagger-options { display: none; }
    #tag-type-checkboxes { display: none; }
    #senses-only-for-penn-poss-checkboxes { display: none; }
    #parser-options { display: none; }
    #extraction-options { display: none; }
   ]]>
  </style>
  <style type="text/css"></style>
  <link rel="stylesheet" type="text/css" href="../style/parser-interface.css" />
 </xsl:template>

 <xsl:template name="parser-interface-options">
  <xsl:if test="@extscontents">
   <label title="The content of the &quot;Extractions&quot; section">Extractions contents: <select id="extscontents" name="extscontents" onchange="setAllDisplay()">
    <option value="raw">
     <xsl:if test="@extscontents = 'raw'">
      <xsl:attribute name="selected">selected</xsl:attribute>
     </xsl:if>
     Raw extractions
    </option>
    <option value="inf">
     <xsl:if test="@extscontents = 'inf'">
      <xsl:attribute name="selected">selected</xsl:attribute>
     </xsl:if>
     Inferred extractions
    </option>
   </select></label>
  </xsl:if>
  <label title="The format of the &quot;Extractions&quot; section">Extractions format: <select id="extsformat" name="extsformat" onchange="setAllDisplay()">
   <option value="lisp">
    <xsl:if test="@extsformat = 'lisp'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    Lisp
   </option>
   <option value="table">
    <xsl:if test="@extsformat = 'table'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    table
   </option>
   <option value="svg">
    <xsl:if test="@extsformat = 'svg'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    SVG diagram
   </option>
  </select></label>
  <label title="The format of the &quot;Tags&quot; section, hidden by default.">Tags format: <select id="tagsformat" name="tagsformat" onchange="setAllDisplay()">
   <option value="hidden">
    <xsl:if test="@tagsformat = 'hidden'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    hidden
   </option>
   <option value="lisp">
    <xsl:if test="@tagsformat = 'lisp'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    Lisp
   </option>
   <option value="table">
    <xsl:if test="@tagsformat = 'table'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    table
   </option>
  </select></label>
  <label title="How much detail to show in the parse tree.">Tree contents: <select id="treecontents" name="treecontents" onchange="setAllDisplay()">
   <option value="full">
    <xsl:if test="@treecontents = 'full'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    Full tree
   </option>
   <option value="phrase">
    <xsl:if test="@treecontents = 'phrase'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    Phrase-level nodes only
   </option>
  </select></label>
  <label title="Which format to show the parse tree in.">Tree format: <select id="treeformat" name="treeformat" onchange="setAllDisplay()">
   <option value="lisp">
    <xsl:if test="@treeformat = 'lisp'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    Lisp
   </option>
   <option value="svg">
    <xsl:if test="@treeformat = 'svg'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    SVG diagram
   </option>
   <option value="LinGO">
    <xsl:if test="@treeformat = 'LinGO'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    LinGO-like table diagram
   </option>
  </select></label>
  <label title="Which format to show the Logical Form terms in.">LF format: <select id="lfformat" name="lfformat" onchange="setAllDisplay()">
   <option value="lisp">
    <xsl:if test="@lfformat = 'lisp'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    Lisp
   </option>
   <option value="amr">
    <xsl:if test="@lfformat = 'amr'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    AMR
   </option>
   <option value="svg">
    <xsl:if test="@lfformat = 'svg'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    SVG diagram
   </option>
  </select></label>
  <label title="Show output written to the Lisp console in the debug section">
   <input type="checkbox" id="debug-checkbox" name="debug" onchange="setAllDisplay()">
    <xsl:if test="@debug">
     <xsl:attribute name="checked">checked</xsl:attribute>
    </xsl:if>
   </input>
   Show debug output
  </label>
 </xsl:template>

 <xsl:template name="texttagger-interface-options">
  <label title="The format of the &quot;Tags&quot; section, table by default.">Tags format: <select id="tagsformat" name="tagsformat" onchange="setAllDisplay()">
   <!-- no "hidden" -->
   <option value="lisp">
    <xsl:if test="@tagsformat = 'lisp'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    Lisp
   </option>
   <option value="table">
    <xsl:if test="@tagsformat = 'table'">
     <xsl:attribute name="selected">selected</xsl:attribute>
    </xsl:if>
    table
   </option>
  </select></label>
  <label title="Show output written to the Lisp console in the debug section">
   <input type="checkbox" id="debug-checkbox" name="debug" onchange="setAllDisplay()">
    <xsl:if test="@debug">
     <xsl:attribute name="checked">checked</xsl:attribute>
    </xsl:if>
   </input>
   Show Lisp debug output
  </label>
 </xsl:template>

 <xsl:template name="interface-options">
  <!-- this doesn't really belong here, but it should be shared between parser-inteface.xsl and drum-interface.xsl -->
  <input type="hidden" name="component">
   <xsl:attribute name="value">
    <xsl:choose>
     <xsl:when test="/trips-parser-output">parser</xsl:when>
     <xsl:otherwise>texttagger</xsl:otherwise>
    </xsl:choose>
   </xsl:attribute>
  </input>
  <xsl:choose>
   <xsl:when test="/trips-parser-output">
    <xsl:call-template name="parser-interface-options" />
   </xsl:when>
   <xsl:otherwise>
    <xsl:call-template name="texttagger-interface-options" />
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>

 <xsl:template match="*" mode="serialize">
  <xsl:text>&lt;</xsl:text>
  <xsl:value-of select="name()" />
  <!-- ick, special case rdf xmlns declarations (which aren't technically attributes so don't match @* ?) -->
  <xsl:if test="self::rdf:RDF">
   <xsl:text>
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  xmlns:role="http://www.cs.rochester.edu/research/trips/role#"
  xmlns:LF="http://www.cs.rochester.edu/research/trips/LF#"</xsl:text>
  </xsl:if>
  <xsl:apply-templates select="@*" mode="serialize" />
  <xsl:choose>
   <xsl:when test="*|text()">
    <xsl:text>&gt;</xsl:text>
    <xsl:apply-templates select="*|text()" mode="serialize" />
    <xsl:text>&lt;/</xsl:text>
    <xsl:value-of select="name()" />
    <xsl:text>&gt;</xsl:text>
   </xsl:when>
   <xsl:otherwise>
    <xsl:text> /&gt;</xsl:text>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>
 
 <xsl:template match="text()" mode="serialize">
  <xsl:variable name="escaped-text-1">
   <xsl:call-template name="str:replace">
    <xsl:with-param name="string" select="string(.)" />
    <xsl:with-param name="search" select="'&amp;'" />
    <xsl:with-param name="replace" select="'&amp;amp;'" />
   </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="escaped-text-2">
   <xsl:call-template name="str:replace">
    <xsl:with-param name="string" select="string(exsl:node-set($escaped-text-1))" />
    <xsl:with-param name="search" select="'&lt;'" />
    <xsl:with-param name="replace" select="'&amp;lt;'" />
   </xsl:call-template>
  </xsl:variable>
  <xsl:value-of select="exsl:node-set($escaped-text-2)" />
 </xsl:template>

 <xsl:template match="@*" mode="serialize">
  <xsl:text> </xsl:text>
  <xsl:value-of select="name()" />
  <xsl:text>="</xsl:text>
  <xsl:variable name="escaped-text-1">
   <xsl:call-template name="str:replace">
    <xsl:with-param name="string" select="string(.)" />
    <xsl:with-param name="search" select="'&amp;'" />
    <xsl:with-param name="replace" select="'&amp;amp;'" />
   </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="escaped-text-2">
   <xsl:call-template name="str:replace">
    <xsl:with-param name="string" select="string(exsl:node-set($escaped-text-1))" />
    <xsl:with-param name="search" select="'&quot;'" />
    <xsl:with-param name="replace" select="'&amp;quot;'" />
   </xsl:call-template>
  </xsl:variable>
  <xsl:value-of select="exsl:node-set($escaped-text-2)" />
  <xsl:text>"</xsl:text>
 </xsl:template>
 
 <xsl:template name="hyps-form">
  <xsl:if test="@input">
   <form action="save.pl" method="POST">
    <xsl:choose>
     <xsl:when test="//alt-hyps">
      <label title="Choose the index of the parsing hypothesis to display.">
       Show parsing hypothesis #
       <input id="hyp" name="hyp" type="number" min="0" max="{/trips-parser-output/@number-parses-desired - 1}" value="0" onchange="displayHyp(parseInt(this.value))" />
      </label>
     </xsl:when>
     <xsl:otherwise>
      <input type="hidden" name="hyp" value="0" />
     </xsl:otherwise>
    </xsl:choose>
    For internal use (login required):
    <input type="submit" name="judgement" value="this hypothesis is correct" title="Save these results, with the judgement that the currently displayed hypothesis is completely correct." />
    <input type="submit" name="judgement" value="all hypotheses are incorrect" title="Save these results, with the judgement that all the available hypotheses are incorrect." /><br />
    <textarea name="comments" placeholder="Comments..." cols="80"><xsl:value-of select="/trips-parser-output/@comments" /></textarea>
    <input type="hidden" name="results">
     <xsl:attribute name="value">
      <xsl:text>&lt;?xml version="1.0" encoding="UTF-8"?&gt;
 &lt;?xml-stylesheet type="text/xsl" href="../style/parser-interface.xsl"?&gt;
 &lt;!DOCTYPE trips-parser-output SYSTEM "../trips-parser-output.dtd"&gt;
 </xsl:text>
      <xsl:apply-templates select="/" mode="serialize" />
     </xsl:attribute>
    </input>
   </form>
  </xsl:if>
 </xsl:template>

 <xsl:template name="footer">
  <hr />
  <div><a href="http://www.cs.rochester.edu/research/trips/lexicon/browse-ont-lex-ajax.html">Browse the TRIPS Lexicon and Ontology</a></div>
  <div><a href="../LF%20Documentation.pdf">LF Documentation (pdf)</a></div>
  <div><a href="../api.html">Web API Documentation</a></div>
  <div>Parser built on
   <xsl:value-of select="@parser-build-date" />
  </div>
  <xsl:if test="@system = 'DRUM' or @system = 'BOB' or contains(@tag-type, 'drum')">
   <p>Some TextTagger tags from the Drum tagger are derived from the <a href="https://www.nlm.nih.gov/mesh/meshhome.html">MeSH&#174;</a> SCR data set from <a href="https://www.nlm.nih.gov/">NLM</a>, but these may not reflect the most current data available from NLM, and NLM has not endorsed this service. See also this <a href="http://trips.ihmc.us/TextTagger/docs/README.xhtml#sec-5.7.9.">table of data sources</a> for the Drum tagger.</p>
  </xsl:if>
  <p>Development of this system has been supported in part by:</p>
  <ul>
   <li>The National Science Foundation (grants 0958193 and 1012205)</li>
   <xsl:choose>
    <xsl:when test="@system = 'DRUM'">
     <li>The Office of Naval Research (grant N000141110417)</li>
     <li>The DARPA Big Mechanism program (grant W911NF-14-11-0391)</li>
    </xsl:when>
    <xsl:when test="@system = 'CWMS'">
     <li>The DARPA World Modelers program under Army Research Office contract W911NF-18-1-0464</li>
    </xsl:when>
    <xsl:otherwise>
     <li>The DARPA CWC program under Army Research Office contract W911NF-15-1-0542</li>
    </xsl:otherwise>
   </xsl:choose>
  </ul>
 </xsl:template>

 <xsl:template match="/trips-parser-output | /texttagger-output">
  <xsl:variable name="title">
   <xsl:text>TRIPS</xsl:text>
   <xsl:if test="@system and not(@system = 'WEB-TOOLS' or @system = 'NIL')">
    <xsl:text>/</xsl:text>
    <xsl:value-of select="@service" />
   </xsl:if>
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
    <xsl:if test="@error">
     <p class="error">Error: <xsl:value-of select="@error" /></p>
    </xsl:if>
    <form action="#">
     <xsl:if test="@system = 'STEP' or @system = 'PROPOLIS' or @service = 'CWMSREADER'">
      <xsl:attribute name="method">POST</xsl:attribute>
     </xsl:if>
     <div>
      <xsl:choose>
       <xsl:when test="@system = 'STEP' or @system = 'PROPOLIS' or @service = 'CWMSREADER'">
        <textarea name="input" placeholder="Enter a paragraph." cols="80" rows="20" maxlength="5000"><xsl:value-of select="@input" /></textarea><br />
       </xsl:when>
       <xsl:otherwise>
        <input type="text" size="40" name="input" value="{@input}" placeholder="Enter a sentence."/>
       </xsl:otherwise>
      </xsl:choose>
      <input type="submit">
       <xsl:attribute name="value">
	<xsl:choose>
	 <xsl:when test="/trips-parser-output">Parse</xsl:when>
	 <xsl:otherwise>Tag</xsl:otherwise>
	</xsl:choose>
       </xsl:attribute>
      </input><br />
      | <a href="javascript:toggleDisplay('interface-options')" title="Click here to show/hide options for how your browser displays the results. These are applied immediately.">Interface options</a>
      <div id="interface-options">
       <xsl:call-template name="interface-options" />
      </div>
      <xsl:if test="@system">
       | <a href="javascript:toggleDisplay('texttagger-options')" title="Click here to show/hide options for the TextTagger preprocessing component. These are applied for the next input you parse.">TextTagger options</a>
       <div id="texttagger-options">
	<label title="Comma-separated list of words for which TextTagger should not add sense information.">No-sense words: <input type="text" id="no-sense-words" name="no-sense-words" value="{@no-sense-words}"/></label> (enter a comma-separated list of words)<br />
	<label title="Lispy boolean expression describing the types of tags TextTagger should ouput. It's recommended to use the checkboxes below, rather than filling this field directly.">Tag type: <input type="text" name="tag-type" id="tag-type" value="{@tag-type}" /></label> (enter a custom boolean tag type expression, or use the <a href="javascript:toggleDisplay('tag-type-checkboxes')" title="Show/hide tagger checkboxes.">tagger checkboxes</a>)<br />
	 <ul class="checkboxes" id="tag-type-checkboxes">
	  <li title="Splits common affixes (just prefixes for now) from words."><label><input type="checkbox" /> affixes</label></li>
	  <li title="Marks sequences of capital letters, numbers, and hyphens as named entities with the type ONT::referential-sem."><label><input type="checkbox" /> alphanumerics</label></li>
	  <xsl:if test="@system = 'DRUM' or @system = 'STEP' or @system = 'BOB' or @system = 'PROPOLIS' or @system = 'WEB-TOOLS'">
	   <li title="Looks up alternate spellings for certain words from a table (mostly US/British differences)."><label><input type="checkbox" /> alternate_spellings</label></li>
	  </xsl:if>
	  <xsl:if test="@system = 'WEB-TOOLS'">
	   <li title="Expands texting acronyms and tags asthma drug names for the TRIPS Asma system."><label><input type="checkbox" /> asma</label></li>
	  </xsl:if>
	  <li title="Marks any capitalized word not at the beginning of a sentence as a named-entity with the type ONT::referential-sem."><label><input type="checkbox" /> capitalized_names</label></li>
	  <li title="Marks chemical formulae (with element abbreviations, counts, bonds, groups, etc.) with type ONT::chemical and Penn POS NNP."><label><input type="checkbox" /> chemical_formulae</label></li>
	  <xsl:if test="@system = 'WEB-TOOLS'">
	   <li title="Uses the Charniak-Johnson Parser (with McClosky's biomed parsing model) to parse sentences, and converts the resulting parse trees to clause, phrase, and POS tags."><label><input type="checkbox" /> cj_parser</label> (all)
	    <ul class="checkboxes">
	     <li><label><input type="checkbox" /> clause</label></li>
	     <li><label><input type="checkbox" /> phrase</label></li>
	     <li><label><input type="checkbox" /> pos</label></li>
	    </ul>
	   </li>
	  </xsl:if>
	  <li title="Splits sentences into clauses at certain punctuation (e.g. commas), as long as the clauses end up long enough, and they agree with other sources of clause tags if enabled."><label><input type="checkbox" /> clauses</label></li>
	  <xsl:if test="@system = 'WEB-TOOLS' or @system = 'STEP'">
	   <li title="Looks up country names and related terms from mledoze/countries' countries.json file."><label><input type="checkbox" /> countries</label></li>
	  </xsl:if>
	  <xsl:if test="@system = 'WEB-TOOLS' or @system = 'DRUM' or @system = 'BOB'">
	   <li title="Looks up biological mechanism terms for the Deep Reader for Understanding Mechanisms (DRUM)."><label><input type="checkbox" /> drum</label></li>
	   <li title="Uses the Enju parser (with the GENIA model) to parse sentences, and converts the resulting parses to clause, phrase, and POS tags."><label><input type="checkbox" /> enju</label> (all)
	    <ul class="checkboxes">
	     <li><label><input type="checkbox" /> clause</label></li>
	     <li><label><input type="checkbox" /> phrase</label></li>
	     <li><label><input type="checkbox" /> pos</label></li>
	    </ul>
	   </li>
	  </xsl:if>
	  <li title="Accepts native tags in the :input-tags argument and integrates them into TextTagger's output as if they originated within TextTagger."><label><input type="checkbox" /> input</label></li>
	  <xsl:if test="@system != 'DRUM' and @system != 'STEP' and @system != 'MUSICA' and @service != 'CWMSREADER'">
	   <li title="Uses GNU Aspell to correct misspellings. Sometimes aspell will split words; in that case this tagger also outputs subword tags. We use the Specialist lexicon for the spelling dictionary."><label><input type="checkbox" /> misspellings</label></li>
	  </xsl:if>
	  <xsl:if test="@system = 'WEB-TOOLS' or @system = 'MUSICA'">
	   <li title="Tags music-related terms and abbreviations, such as pitches, intervals, and chords."><label><input type="checkbox" /> music</label></li>
	  </xsl:if>
	  <xsl:if test="@system = 'DRUM' or @system = 'STEP' or @system = 'PROPOLIS'">
	   <li title="Tags nonempty lines as sentences."><label><input type="checkbox" /> one_sentence_per_line</label></li>
	  </xsl:if>
	  <xsl:if test="@system = 'WEB-TOOLS' or @system = 'STEP'">
	   <li title="Tags personal names, with their gender if it's not too ambiguous. Uses the SSA names list."><label><input type="checkbox" /> personal_names</label></li>
	  </xsl:if>
	  <xsl:if test="@system = 'WEB-TOOLS' or @system = 'CWMS'">
	   <li title="Looks up place names from several resources (including those of the Countries and Terms taggers, among others), in a similar way to the Drum tagger."><label><input type="checkbox" /> place_names</label></li>
	  </xsl:if>
	  <li title="Tags certain (often abbreviated) phrases occurring in prescriptions as either adjectives or adverbs."><label><input type="checkbox" /> prescriptions</label></li>
	  <li title="Tags each punctuation character."><label><input type="checkbox" /> punctuation</label></li>
	  <li title="Tags double-quoted strings."><label><input type="checkbox" /> quotations</label></li>
	  <li title="Tags roman numerals as numbers."><label><input type="checkbox" /> roman_numerals</label></li>
	  <li title="Uses the Lingua::EN::Sentence Perl module to split the string into sentences."><label><input type="checkbox" /> sentences</label></li>
	  <li title="Splits the string on sequences of whitespace longer than one character."><label><input type="checkbox" /> spaced_chunks</label></li>
	  <xsl:if test="@system = 'WEB-TOOLS' or @system = 'DRUM' or @system = 'BOB'">
	   <li title="Adds information from the SPECIALIST lexicon, including part of speech, citation form, complement patterns, and derivational links."><label><input type="checkbox" /> specialist</label></li>
	  </xsl:if>
	  <xsl:if test="@system != 'CABOT' and @system != 'COGENT' and @system != 'MUSICA'">
	   <li title="Uses Stanford CoreNLP to process the string (or each sentence tag if they're available), using the tokenizer to produce word and punctuation tags, the sentence splitter to produce sentence tags (unless they were already in the input), the POS tagger to produce pos tags, the parser to produce clause and phrase tags, and the NER to produce named-entity tags."><label><input type="checkbox" /> stanford_core_nlp</label> (all)
	    <ul class="checkboxes">
	     <li><label><input type="checkbox" /> clause</label></li>
	     <li><label><input type="checkbox" /> named-entity</label></li>
	     <li><label><input type="checkbox" /> phrase</label></li>
	     <li><label><input type="checkbox" /> pos</label></li>
	     <li><label><input type="checkbox" /> punctuation</label></li>
	     <li><label><input type="checkbox" /> sentence</label></li>
	     <li><label><input type="checkbox" /> word</label></li>
	    </ul>
	   </li>
	  </xsl:if>
	  <li title="Uses the Geo::StreetAddress::US Perl module to tag anything that could be the first line of a US address (e.g. &quot;40 S. Alcaniz St.&quot;)."><label><input type="checkbox" /> street_addresses</label></li>
	  <xsl:if test="@system = 'WEB-TOOLS' or @system = 'STEP'">
	   <li title="Tags any substring that matches a name in the GNIS database as a named-entity with type ONT::geographic-region."><label><input type="checkbox" /> terms</label> (US geographic names)</li>
	  </xsl:if>
	  <li title="Tags anything matching the :lex (or :text) argument of any of the tags from the :input-terms list with a copy of the matching tag(s)."><label><input type="checkbox" /> terms_input</label></li>
	  <li title="Tags units of measure as nouns with lftypes under MEASURE-UNIT according to their dimensions."><label><input type="checkbox" /> units</label></li>
	  <li title="Tags non-first variants in slash- or ampersand-separated lists of variants like &quot;Smad1/5/8&quot; as alternate spellings that include the prefix, like &quot;5&quot; -> &quot;Smad5&quot; and &quot;8&quot; -> &quot;Smad8&quot;. The Words tagger should already get the first one, &quot;Smad1&quot;."><label><input type="checkbox" /> variant_lists</label></li>
	  <li title="Tags multi-word lexical items from WordNet with their sense key, part of speech, and morphology."><label><input type="checkbox" /> word_net</label></li>
	  <li title="Tags anything that the Parser might want to consider a separate word. That includes splitting CamelCase and contractions, among other things."><label><input type="checkbox" /> words</label> (highly recommended!)</li>
	  <xsl:if test="@system != 'STEP'">
	   <li>Please note that in this version of the web parser, even if you select one of the sentence- or clause- splitting taggers, the TRIPS Parser will still see the whole input as one "utterance" and may not decide to split it in the same way, or at all. If you want it to split the same way, use <a href="drum">the drum paragraph parser</a> instead.</li>
	  </xsl:if>
	 </ul>
	<label>Input tags:<br/>
	 <textarea name="input-tags" id="input-tags" placeholder="Enter a Lispy list of native tags with :start/:end."><xsl:value-of select="@input-tags" /></textarea></label><br/>
	<label>Input terms:<br/>
	 <textarea name="input-terms" id="input-terms" placeholder="Enter a Lispy list of native tags without :start/:end."><xsl:value-of select="@input-terms" /></textarea></label><br/>
	<label>Output sense information only for words with Penn POS tags: <input type="text" name="senses-only-for-penn-poss" id="senses-only-for-penn-poss" value="{@senses-only-for-penn-poss}" /></label> (enter a comma-separated list of tags, or use the <a href="javascript:toggleDisplay('senses-only-for-penn-poss-checkboxes')" title="Show/hide Penn POS checkboxes.">checkboxes</a>)<br />
	 <ul class="checkboxes" id="senses-only-for-penn-poss-checkboxes">
	  <li><label><input type="checkbox" /> all noun tags</label>
	   <ul class="checkboxes">
	    <li><label><input type="checkbox" /> NN</label>  singular or mass</li>
	    <li><label><input type="checkbox" /> NNS</label>  plural</li>
	    <li><label><input type="checkbox" /> NNP</label>  proper singular</li>
	    <li><label><input type="checkbox" /> NNPS</label>  proper plural</li>
	   </ul>
	  </li>
	  <li><label><input type="checkbox" /> all verb tags</label>
	   <ul class="checkboxes">
	    <li><label><input type="checkbox" /> MD</label>  modal</li>
	    <li><label><input type="checkbox" /> VB</label>  base form</li>
	    <li><label><input type="checkbox" /> VBD</label>  past tense</li>
	    <li><label><input type="checkbox" /> VBG</label>  gerund or present participle</li>
	    <li><label><input type="checkbox" /> VBN</label>  past participle</li>
	    <li><label><input type="checkbox" /> VBP</label>  non-3rd person singular present</li>
	    <li><label><input type="checkbox" /> VBZ</label>  3rd person singular present</li>
	   </ul>
	  </li>
	  <li><label><input type="checkbox" /> all adjective tags</label>
	   <ul class="checkboxes">
	    <li><label><input type="checkbox" /> JJ</label></li>
	    <li><label><input type="checkbox" /> JJR</label>  comparative</li>
	    <li><label><input type="checkbox" /> JJS</label>  superlative</li>
	   </ul>
	  </li>
	  <li><label><input type="checkbox" /> all adverb tags</label>
	   <ul class="checkboxes">
	    <li><label><input type="checkbox" /> RB</label></li>
	    <li><label><input type="checkbox" /> RBR</label>  comparative</li>
	    <li><label><input type="checkbox" /> RBS</label>  superlative</li>
	   </ul>
	  </li>
	  <li><label><input type="checkbox" /> all other tags</label>
	   <ul class="checkboxes">
	    <li><label><input type="checkbox" /> CC</label>  coordinating conjunction</li>
	    <li><label><input type="checkbox" /> CD</label>  cardinal number</li>
	    <li><label><input type="checkbox" /> DT</label>  determiner</li>
	    <li><label><input type="checkbox" /> EX</label>  existential there</li>
	    <li><label><input type="checkbox" /> FW</label>  foreign word</li>
	    <li><label><input type="checkbox" /> IN</label>  preposition or subordinating conjunction</li>
	    <li><label><input type="checkbox" /> LS</label>  list item marker</li>
	    <li><label><input type="checkbox" /> PDT</label>  predeterminer</li>
	    <li><label><input type="checkbox" /> POS</label>  possessive ending</li>
	    <li><label><input type="checkbox" /> PRP</label>  personal pronoun</li>
	    <li><label><input type="checkbox" /> PRP$</label>  possessive pronoun</li>
	    <li><label><input type="checkbox" /> RP</label>  particle</li>
	    <li><label><input type="checkbox" /> SYM</label>  symbol</li>
	    <li><label><input type="checkbox" /> TO</label>  to</li>
	    <li><label><input type="checkbox" /> UH</label>  interjection</li>
	    <li><label><input type="checkbox" /> WDT</label>  wh-determiner</li>
	    <li><label><input type="checkbox" /> WP</label>  wh-pronoun</li>
	    <li><label><input type="checkbox" /> WP$</label>  possessive wh-pronoun</li>
	    <li><label><input type="checkbox" /> WRB</label>  wh-adverb</li>
	   </ul>
	   (note that . , ; `` '' are omitted for technical reasons, but they don't usually have sense tags in the first place)
	  </li>
	 </ul>
        <xsl:if test="@system = 'STEP' or @system = 'DRUM' or @system = 'PROPOLIS' or @service = 'CWMSREADER'">
	 <label title="Select how finely TextTagger splits the paragraph into utterances; 'split sentences' only splits on sentence breaks, while 'split clauses' additionally splits on commas and certain other punctuation if the resulting clauses are long enough. Note that the Parser may further split the utterances it gets from TextTagger into smaller fragments, regardless of this setting.">Split mode: <select name="split-mode" id="split-mode">
	  <option value="split-clauses">
	   <xsl:if test="@split-mode = 'split-clauses'">
	    <xsl:attribute name="selected">selected</xsl:attribute>
	   </xsl:if>
	   <xsl:text>split clauses</xsl:text>
	  </option>
	  <option value="split-sentences">
	   <xsl:if test="@split-mode = 'split-sentences'">
	    <xsl:attribute name="selected">selected</xsl:attribute>
	   </xsl:if>
	   <xsl:text>split sentences</xsl:text>
	  </option>
	 </select></label>
        </xsl:if>
       </div>
      </xsl:if>
      <xsl:if test="/trips-parser-output">
       <xsl:if test="@system = 'STEP' or @number-parses-desired">
	| <a href="javascript:toggleDisplay('parser-options')" title="Click here to show/hide options for the main Parser component. These are applied for the next input you parse.">Parser options</a>
	<div id="parser-options">
	 <xsl:if test="@system = 'STEP'">
	  <ul class="checkboxes" id="parser-checkboxes">
	   <li><label><input type="checkbox" name="semantic-skeleton-scoring">
	    <xsl:if test="@semantic-skeleton-scoring">
	     <xsl:attribute name="checked">checked</xsl:attribute>
	    </xsl:if>
	   </input>
	   semantic skeleton scoring</label></li>
	  </ul>
	 </xsl:if>
	 <xsl:if test="@number-parses-desired">
	  <label>Number of parsing hypotheses desired: <input type="number" name="number-parses-desired" min="1" max="20" value="{@number-parses-desired}" /></label>
	 </xsl:if>
	</div>
       </xsl:if>
       <xsl:if test="@system">
	| <a href="javascript:toggleDisplay('extraction-options')" title="Click here to show/hide options for the NewIM component, which handles extraction rules. These are applied for the next input you parse.">Extraction options</a>
	<div id="extraction-options">
	 <label><input type="checkbox" name="trace-level" value="3">
	  <xsl:if test="@trace-level">
	   <xsl:attribute name="checked">checked</xsl:attribute>
	  </xsl:if>
	 </input> trace extractions in debug output</label><br/>
	 <label>rule set:
	  <select name="rule-set">
	   <option value="">
	    <xsl:if test="not(@rule-set) or @rule-set=''">
	     <xsl:attribute name="selected">selected</xsl:attribute>
	    </xsl:if>
	    <xsl:text>[default]</xsl:text>
	   </option>
	   <xsl:variable name="rule-set" select="@rule-set" />
	   <xsl:variable name="rule-sets">
	    <xsl:call-template name="str:tokenize"> <!-- imported from tags-to-table.xsl, which is included here -->
	     <xsl:with-param name="string" select="@rule-sets" />
	     <xsl:with-param name="delimiters" select="','" />
	    </xsl:call-template>
	   </xsl:variable>
	   <xsl:for-each select="exsl:node-set($rule-sets)/token">
	    <option>
	     <xsl:if test="$rule-set=string(.)">
	      <xsl:attribute name="selected">selected</xsl:attribute>
	     </xsl:if>
	     <xsl:value-of select="." />
	    </option>
	   </xsl:for-each>
	  </select>
	 </label>
	</div>
       </xsl:if>
      </xsl:if>
      |
     </div>
    </form>
    <xsl:call-template name="hyps-form" />
    <xsl:apply-templates />
    <xsl:call-template name="footer" />
   </body>
  </html>
 </xsl:template>

 <xsl:template match="/">
  <xsl:choose>
   <xsl:when test="$mode = 'tags-to-table'">
    <xsl:apply-templates select="tags" mode="tags-to-table" />
   </xsl:when>
   <xsl:when test="$mode = 'tree-to-LinGO'">
    <xsl:apply-templates select="*" mode="tree-to-LinGO" />
   </xsl:when>
   <xsl:when test="$mode = 'tree-to-dot'">
    <xsl:apply-templates select="*" mode="tree-to-dot" />
   </xsl:when>
   <xsl:when test="$mode = 'lf-to-amr'">
    <xsl:apply-templates select="rdf:RDF" mode="lf-to-amr" />
   </xsl:when>
   <xsl:when test="$mode = 'lf-to-dot'">
    <xsl:apply-templates select="rdf:RDF" mode="lf-to-dot" />
   </xsl:when>
   <xsl:when test="$mode = 'exts-to-lisp'">
    <xsl:apply-templates select="ekb" mode="exts-to-lisp" />
   </xsl:when>
   <xsl:when test="$mode = 'exts-to-table'">
    <xsl:apply-templates select="ekb" mode="exts-to-table" />
   </xsl:when>
   <xsl:when test="$mode = 'exts-to-rdf'">
    <xsl:apply-templates select="ekb" mode="exts-to-rdf" />
   </xsl:when>
   <xsl:otherwise>
    <xsl:apply-templates select="*" />
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>

</xsl:stylesheet>
