<?xml version="1.0" encoding="UTF-8"?>
<stylesheet version="1.0"
    xmlns="http://www.w3.org/1999/XSL/Transform"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:role="http://www.cs.rochester.edu/research/trips/role#"
    xmlns:exsl="http://exslt.org/common"
    xmlns:str="http://exslt.org/strings"
    extension-element-prefixes="exsl str">

<!--
ekb-to-dot.xsl - convert an ekb file to Graphviz dot format
William de Beaumont
2016-03-11

Note: this isn't used by the website. It's meant for the case when you have a
local .ekb file you want to view as a graph.

To install:
make -f Makefile-component install

To use:
xsltproc $TRIPS_BASE/www/style/ekb-to-dot.xsl input.ekb >output.dot

To view output.dot on a Mac, open it with the Graphviz app:
open -a Graphviz output.dot

To view output.dot on Linux:
dot -Tpng output.dot |display -

To get a .png file as close to what the website shows as possible (remove the
space in "- -param", XML doesn't like double dashes):
cat input.ekb \
|xsltproc - -param group-by-utts 'false()' $TRIPS_BASE/www/style/ekb-to-dot.xsl - \
|ccomps -x |dot |gvpack -array1 |neato -Tpng -n2 \
>output.png

To do vertical layout but keep the utterance grouping and labeling (note
"-array1" changed to "-array_u1"):
cat input.ekb \
|xsltproc $TRIPS_BASE/www/style/ekb-to-dot.xsl - \
|dot |gvpack -array_u1 |neato -Tpng -n2 \
>output.png

For more on vertical layout, see:
http://stackoverflow.com/questions/8002352/how-to-control-subgraphs-layout-in-dot
  -->

<import href="exts-to-rdf.xsl" />
<import href="lf-to-dot.xsl" />

<output method="text" encoding="UTF-8" />

<!-- turn this off to more closely match the behavior of the website -->
<param name="group-by-utts" select="true()" />

<key name="utts" match="/ekb/*" use="@uttnum" />

<template match="rdf:RDF" mode="ekb-to-dot">
 <param name="headers"><text></text></param>
 <text>digraph LFGraph {
  node [shape=none]
</text>
 <copy-of select="$headers" />
 <apply-templates mode="lf-to-dot" />
 <text>}
</text>
</template>

<template match="/">
 <choose>
  <when test="$group-by-utts">
   <for-each select="ekb/*[@uttnum and generate-id() = generate-id(key('utts', @uttnum)[1])]">
    <variable name="rdf">
     <rdf:RDF>
      <apply-templates select="key('utts', @uttnum)" mode="exts-to-rdf" />
      <!-- special case for meta-role mod on various roles -->
      <for-each select="key('utts', @uttnum)//*[@mod]">
       <rdf:Description rdf:about="#{generate-id()}">
	<role:mod><value-of select="@mod" /></role:mod>
       </rdf:Description>
      </for-each>
     </rdf:RDF>
    </variable>
    <apply-templates select="exsl:node-set($rdf)" mode="ekb-to-dot">
     <with-param name="headers">
      <!-- sort graphs by uttnum -->
      <text>  sortv=</text>
      <value-of select="@uttnum" />
      <!-- add numbered utterance text node -->
      <text>
  "utt</text>
      <value-of select="@uttnum" />
      <text>" [label="</text>
      <value-of select="@uttnum" />
      <text>. </text>
      <variable name="uttnum" select="@uttnum" />
      <variable name="escaped-text">
       <call-template name="str:replace">
	<with-param name="string" select="/ekb/input/sentences/sentence[@id=$uttnum]" />
	<with-param name="search" select="'&quot;'" />
	<with-param name="replace" select="'\&quot;'" />
       </call-template>
      </variable>
      <value-of select="exsl:node-set($escaped-text)" />
      <text>"]
</text>
      <!-- add invisible edges from that node to each main node in the group -->
      <for-each select="key('utts', @uttnum)">
       <sort select="@start" data-type="number" />
       <sort select="@end" data-type="number" />
       <text>  "utt</text>
       <value-of select="$uttnum" />
       <text>" -> "</text>
       <value-of select="@id" />
       <text>" [style="invis"]
</text>
      </for-each>
     </with-param>
    </apply-templates>
   </for-each>
  </when>
  <otherwise> <!-- no grouping -->
   <variable name="rdf">
    <apply-templates mode="exts-to-rdf" />
   </variable>
   <apply-templates select="exsl:node-set($rdf)" mode="ekb-to-dot" />
  </otherwise>
 </choose>
</template>

</stylesheet>

