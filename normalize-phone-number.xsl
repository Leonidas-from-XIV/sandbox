<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <!-- http://stackoverflow.com/a/3067130-->
  <xsl:template name="string-replace-all">
    <xsl:param name="text"/>
    <xsl:param name="replace"/>
    <xsl:param name="by"/>
    <xsl:choose>
      <xsl:when test="contains($text, $replace)">
        <xsl:value-of select="substring-before($text,$replace)"/>
        <xsl:value-of select="$by"/>
        <xsl:call-template name="string-replace-all">
          <xsl:with-param name="text" select="substring-after($text,$replace)"/>
          <xsl:with-param name="replace" select="$replace"/>
          <xsl:with-param name="by" select="$by"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="@* | node()">
    <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="sms/@address">
    <xsl:attribute name="address">
      <xsl:call-template name="string-replace-all">
        <xsl:with-param name="text" select="."/>
        <xsl:with-param name="replace" select="' '"/>
        <xsl:with-param name="by" select="''"/>
      </xsl:call-template>
    </xsl:attribute>
  </xsl:template>

</xsl:transform>
