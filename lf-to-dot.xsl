<?xml version="1.0" encoding="UTF-8"?>
<stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/XSL/Transform"
    xmlns:exsl="http://exslt.org/common"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="str exsl"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:role="http://www.cs.rochester.edu/research/trips/role#"
    xmlns:TMA="http://www.cs.rochester.edu/research/trips/TMA#"
    xmlns:LF="http://www.cs.rochester.edu/research/trips/LF#">

<!--
lf-to-dot.xsl - convert an LF (in RDF format, as output by lf-to-rdf in src/LFEvaluator/lf-to-rdf.lisp) to Graphviz dot format
William de Beaumont
2014-06-14
  -->    

<import href="str.replace.template.xsl" />

<output method="xml" encoding="UTF-8" />

<template match="text()|@*" mode="lf-to-dot" />

<!-- role edges and non-term nodes -->
<template match="role:*[local-name() != 'START' and local-name() != 'END' and local-name() != 'DRUM' and local-name() != 'WORDNET']" mode="lf-to-dot" >
 <!-- get the node ID of the role value -->
 <variable name="val-id">
  <choose>
   <when test="@rdf:resource">
    <value-of select="substring(@rdf:resource,2)" />
   </when>
   <when test="rdf:Description/@rdf:ID">
    <value-of select="rdf:Description/@rdf:ID" />
   </when>
   <otherwise>
    <value-of select="generate-id()" />
   </otherwise>
  </choose>
 </variable>
 <!-- make the node for the role value if necessary -->
 <choose>
  <when test="rdf:Description/@rdf:ID">
   <apply-templates select="rdf:Description" mode="lf-to-dot" />
  </when>
  <when test="not(@rdf:resource)">
   <text>  "</text>
   <value-of select="$val-id" />
   <text>" [label="</text>
   <variable name="escaped-text">
    <call-template name="str:replace">
     <with-param name="string" select="text()" />
     <with-param name="search" select="'&quot;'" />
     <with-param name="replace" select="'\&quot;'" />
    </call-template>
   </variable>
   <value-of select="exsl:node-set($escaped-text)" />
   <text>"]
</text>
  </when>
 </choose>
 <!-- make the edge -->
 <text>  "</text>
 <value-of select="../@rdf:ID" />
 <text>" -&gt; "</text>
 <value-of select="$val-id" />
 <text>" [label=":</text>
 <value-of select="local-name()" />
 <!-- add the mod on the role, if any, to its edge label, in parens -->
 <if test="@rdf:ID">
  <variable name="role-id" select="concat('#',@rdf:ID)" />
  <variable name="mod" select="../../rdf:Description[@rdf:about = $role-id]/role:mod" />
  <if test="$mod">
   <text> (</text>
   <value-of select="$mod" />
   <text>)</text>
  </if>
 </if>
 <text>"]
</text>
</template>

<template match="rdf:Description[@rdf:ID]" mode="lf-to-dot">
 <!-- term node -->
 <text>  "</text>
 <value-of select="@rdf:ID" />
 <text>" [label=&lt;(</text>
 <value-of select="LF:indicator" />
 <text> </text>
 <choose>
  <when test="LF:word">
   <text>(:* </text>
   <value-of select="LF:type" />
   <text> &lt;B&gt;</text>
   <value-of select="LF:word" />
   <text>&lt;/B&gt;)</text>
  </when>
  <otherwise>
   <value-of select="LF:type" />
  </otherwise>
 </choose>
 <text>)&gt;,fontcolor="</text>
 <choose>
  <!-- relational/predicate-like things are warm colors -->
  <when test="contains(' EVENT F ', concat(' ', LF:indicator, ' '))"><!-- plain old events and relations -->
   <text>#800000</text>
  </when>
  <when test="contains(' CC EPI MODALITY ', concat(' ', LF:indicator, ' '))"><!-- relations between events -->
   <text>#804000</text>
  </when>
  <when test="LF:indicator = 'SPEECHACT'"><!-- starting point -->
   <text>#c06000</text>
  </when>
  <!-- noun/term-like things are cool colors -->
  <when test="contains(' TERM THE A PRO KIND QUANTIFIER WH-TERM ', concat(' ', LF:indicator, ' '))"><!-- singular-ish things -->
   <text>#000080</text>
  </when>
  <when test="contains(' aggregate THE-SET INDEF-SET SM PRO-SET BARE ', concat(' ', LF:indicator, ' '))"><!-- plural/mass-ish things -->
   <text>#008080</text>
  </when>
  <when test="LF:indicator = 'IMPRO'"><!-- "greyed out" for not being in the input -->
   <text>#404080</text>
  </when>
  <!-- everything else stays black -->
  <otherwise>
   <text>black</text>
  </otherwise>
 </choose>
 <text>"]
</text>

 <apply-templates select="role:*" mode="lf-to-dot" />
</template>

<template match="rdf:Description[@rdf:about]" mode="lf-to-dot">
 <!-- suppress Descriptions for roles with meta-roles -->
</template>

<template match="rdf:RDF" mode="lf-to-dot">
 <html:pre class="dot">
  <text>digraph LFGraph {
  node [shape=none]
</text>
  <apply-templates mode="lf-to-dot" />
  <text>}
</text>
 </html:pre>
</template>

</stylesheet>

