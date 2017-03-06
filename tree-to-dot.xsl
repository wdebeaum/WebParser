<?xml version="1.0" encoding="UTF-8"?>
<stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/XSL/Transform"
    xmlns:html="http://www.w3.org/1999/xhtml">
<!-- tree-to-dot.xsl - convert an XML tree structure to Graphviz dot format
William de Beaumont
2010-02-03
  -->
<output method="xml" encoding="UTF-8" />

<template match="text()|@*" mode="tree-to-dot" />

<template match="node()" mode="node-to-dot">
 <text>"</text>
 <value-of select="generate-id()" />
 <text>"</text>
</template>

<template match="*[not(parent::*) or local-name(parent::*) = 'tree']" mode="tree-to-dot">
 <html:pre class="dot">
  <!-- header -->
  <text>digraph ParseTree {
  graph [nodesep="0.14",ranksep="0.14"]
  node [shape="none",margin="0,0", height="0"]
  edge [arrowhead="none"]
</text>
  <!-- nodes -->
  <for-each select="descendant-or-self::node()">
   <text>  </text>
   <apply-templates select="." mode="node-to-dot" />
   <text> [label="</text>
   <choose>
    <when test="self::text()">
     <value-of select="." />
    </when>
    <otherwise>
     <value-of select="local-name()" />
    </otherwise>
   </choose>
   <text>",rank="sink"]
</text>
  </for-each>
  <!-- edges -->
  <for-each select="descendant-or-self::*">
   <variable name="parent" select="." />
   <for-each select="node()">
    <variable name="child" select="." />
    <text>  </text>
    <apply-templates select="$parent" mode="node-to-dot" />
    <text> -&gt; </text>
    <apply-templates select="$child" mode="node-to-dot" />
    <text>
</text>
   </for-each>
  </for-each>
  <!-- footer -->
  <text>}
</text>
 </html:pre>
</template>

</stylesheet>
