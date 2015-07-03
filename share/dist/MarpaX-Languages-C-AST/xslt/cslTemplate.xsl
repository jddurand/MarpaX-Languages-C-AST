<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:csl="urn:csl">
  <xsl:output method="text" omit-xml-declaration="yes" />

  <!-- =================================================================== -->
  <!--                            MAIN                                     -->
  <!-- =================================================================== -->

  <xsl:template match="/csl">
/* First build a list of all identifiers */
const char **<xsl:value-of select="csl:prefix()"/>_identifiers[] = {
    <xsl:for-each select="./identifiers">
      <xsl:call-template name="identifiers">
        <xsl:with-param name="base" select="''"/>
      </xsl:call-template>
    </xsl:for-each>
  NULL
}
  </xsl:template>

  <!-- =================================================================== -->
  <!--                         IDENTIFIERS                                 -->
  <!-- =================================================================== -->
  <xsl:template name="identifiers">
    <xsl:param name="base"/>
    <xsl:for-each select="./identifier">
      <xsl:call-template name="identifier">
        <xsl:with-param name="base" select="$base"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                         IDENTIFIER                                  -->
  <!-- =================================================================== -->
  <xsl:template name="identifier">
    <xsl:param name="base"/>
    <xsl:variable name="fullIdentifier">
      <xsl:choose>
        <xsl:when test="$base!=''">
          <xsl:value-of select="concat($base, '.', ./@text)" />
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="./@text" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    "<xsl:value-of select="$fullIdentifier" />",
    <!-- inner identifiers -->
    <xsl:for-each select="./identifiers">
      <xsl:call-template name="identifiers">
        <xsl:with-param name="base" select="$fullIdentifier"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
