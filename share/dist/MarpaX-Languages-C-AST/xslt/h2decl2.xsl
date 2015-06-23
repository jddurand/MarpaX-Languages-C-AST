<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:hsl="urn:hsl">
  <xsl:output method="text" omit-xml-declaration="yes" />

  <!-- =================================================================== -->
  <!--                                  MAIN                               -->
  <!-- =================================================================== -->

  <xsl:template match="/">
    <!-- we handle only top-level declarations, i.e. externalDeclaration/declaration -->
    <xsl:for-each select="./translationUnit/externalDeclaration/declaration">
      <xsl:call-template name="declaration" />
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                               declaration                           -->
  <!-- =================================================================== -->
  <xsl:template name="declaration">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <!--
        We are interested only by the first declarator. Recursivity can occur.
    -->
    <xsl:for-each select=".//*[local-name()='declarator' and position()=1]">
      <!--
          Per def a declarator has a directDeclaratorIdentifier. The first is the one we are looking for
      -->     
      <xsl:variable name="IDENTIFIER" select=".//*[local-name()='IDENTIFIER' and position()=1]" />
      <xsl:call-template name="declarator">
        <xsl:with-param name="IDENTIFIER" select="$IDENTIFIER" />
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                declarator                           -->
  <!-- =================================================================== -->
  <xsl:template name="declarator">
    <xsl:param name="IDENTIFIER" />
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s(IDENTIFIER: %s): %s', local-name(), $IDENTIFIER/@text, @text)" />
    <xsl:text>declare </xsl:text><xsl:value-of select="$IDENTIFIER/@text" /><xsl:text> as </xsl:text>
    <xsl:for-each select="./pointer">
      <xsl:call-template name="pointer" />
    </xsl:for-each>
<xsl:text>
</xsl:text>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 pointer                             -->
  <!-- =================================================================== -->
  <xsl:template name="pointer">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:text>pointer to </xsl:text>
    <xsl:for-each select="./pointerQualifierList">
      <xsl:call-template name="pointerQualifierList" />
    </xsl:for-each>
    <xsl:for-each select="./pointer">
      <xsl:call-template name="pointer" />
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                           pointerQualifierList                      -->
  <!-- =================================================================== -->
  <xsl:template name="pointerQualifierList">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./pointerQualifier">
      <xsl:call-template name="pointerQualifier" />
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                             pointerQualifier                        -->
  <!-- =================================================================== -->
  <xsl:template name="pointerQualifier">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./typeQualifier">
      <xsl:call-template name="typeQualifier" />
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                              typeQualifier                          -->
  <!-- =================================================================== -->
  <xsl:template name="typeQualifier">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:value-of select="concat(./@text, ' ')" />
  </xsl:template>

</xsl:stylesheet>
