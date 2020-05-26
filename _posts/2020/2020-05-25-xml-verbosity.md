---
layout: post
title: "XML Verbosity"
categories: [ computing ]
---

XML gets a bad rap for being verbose and tedious,
but over time the associated tools
have only gotten nicer.  These days, it's pretty easy to define
a looser and friendlier schema, and let `XSLT` recover the detail.

For example, I can write a table in Github-Flavored markdown
like this:

~~~~~ markdown
| Name    | Tax Date   | Count |
|---------|------------|-------|
| Richard | 2020-06-15 | 25    |
| Tim     | 2020-04-15 | 13    |
~~~~~

The equivalent XML table _could_ be very
verbose, with tags and metadata around every cell, and for many
uses that would be acceptable.  But, if I'm 
only marking it up for display, I can easily get away
with something more like this:

~~~~~~ xml
<tbl>
  <h>Name    |Tax Date  |Count</h>
  <r>Richard |2020-06-15|25   </r>
  <r>Tim     |2020-04-15|13   </r>
</tbl>
~~~~~~

... and modern `XSLT 3.0` has no trouble splitting the rows
and generating all the needed `<th>` and `<td>` tags
when converting it to `HTML`.  Here's one way to do it:

~~~~~~ xml
<xsl:template match="post:tbl">
    <table>
        <xsl:apply-templates />
    </table>
</xsl:template>

<xsl:template match="post:tbl/post:h">
    <tr>
        <xsl:for-each select="tokenize(.,'\|')">
            <th><xsl:apply-templates select="normalize-space(.)" /></th>
        </xsl:for-each>
    </tr>
</xsl:template>

<xsl:template match="post:tbl/post:r">
    <tr>
        <xsl:for-each select="tokenize(.,'\|')">
            <td><xsl:apply-templates select="normalize-space(.)" /></td>
        </xsl:for-each>
    </tr>
</xsl:template>
~~~~~~

In fact, if I wanted to work a lot harder on the `XSLT`, 
I'm pretty sure I could allow writing the literal same markdown
fragment inside `<tbl>` tags. I can turn the dial as far as I
want toward a loose or strict representation, as my needs dictate.

It's tempting to think that maybe with some effort, I could
arrive at a schema+stylesheet which makes XML blogging as
convenient as markdown blogging. Looking over this post as I
write it, I think the biggest obstacle to that would be how
many `<![CDATA[]]>` sections I would need.  That would get
ugly fast!