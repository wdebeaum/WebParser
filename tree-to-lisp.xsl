<?xml version="1.0" encoding="UTF-8"?>
<stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/XSL/Transform">
<!-- tree-to-lisp.xsl - convert an XML tree structure back to Lisp format
William de Beaumont
2010-02-03
  -->

<output method="xml" encoding="UTF-8" />

<template match="*" mode="tree-to-lisp">
 <text>(</text>
 <value-of select="local-name()" />
 <for-each select="node()">
  <text> </text>
  <apply-templates select="." mode="tree-to-lisp" />
 </for-each>
 <text>)</text>
</template>

<template match="text()" mode="tree-to-lisp">
 <value-of select="." />
</template>

</stylesheet>
