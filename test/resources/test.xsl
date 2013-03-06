<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:param name="format" select="'Aaron'"/>
  <xsl:template match="/">
    <html>
      <body>
	      <xsl:value-of select="$format"/>
        <h2>My CD Collection</h2>
        <table border="1">
          <tr bgcolor="#9acd32">
		  <th><xsl:value-of select="'TEST'"/></th>
            <th>Artist</th>
          </tr>
          <xsl:for-each select="catalog/cd">
            <tr>
              <td>
                <xsl:value-of select="title" />
              </td>
              <td>
                <xsl:value-of select="artist" />
              </td>
            </tr>
          </xsl:for-each>
        </table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
