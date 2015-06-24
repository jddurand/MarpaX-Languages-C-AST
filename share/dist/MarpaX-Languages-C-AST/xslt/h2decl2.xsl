<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:hsl="urn:hsl">
  <xsl:output method="text" omit-xml-declaration="yes" />

  <!-- =================================================================== -->
  <!--                                  MAIN                               -->
  <!-- =================================================================== -->

  <xsl:template match="/">
    <!-- Select only declarations -->
    <xsl:for-each select="./translationUnit/externalDeclaration/declaration" >
      <xsl:call-template name="declaration" />
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                              declaration                            -->
  <!-- =================================================================== -->
  <xsl:template name="declaration">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='declarationSpecifiers'" >
          <xsl:call-template name="declarationSpecifiers" />
        </xsl:when>
        <xsl:when test="local-name()='declarationCheck'" >
          <xsl:call-template name="declarationCheck" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                          declarationSpecifiers                      -->
  <!-- =================================================================== -->
  <xsl:template name="declarationSpecifiers">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='declarationSpecifiers0'" >
          <xsl:call-template name="declarationSpecifiers0" />
        </xsl:when>
        <xsl:when test="local-name()='declarationSpecifiers1'" >
          <xsl:call-template name="declarationSpecifiers1" />
        </xsl:when>
        <xsl:when test="local-name()='declarationSpecifiers2'" >
          <xsl:call-template name="declarationSpecifiers2" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                             declarationCheck                        -->
  <!-- =================================================================== -->
  <xsl:template name="declarationCheck">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='declarationCheckdeclarationSpecifiers'" >
          <xsl:call-template name="declarationCheckdeclarationSpecifiers" />
        </xsl:when>
        <xsl:when test="local-name()='declarationCheckinitDeclaratorList'" >
          <xsl:call-template name="declarationCheckinitDeclaratorList" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                           declarationSpecifiers0                    -->
  <!-- =================================================================== -->
  <xsl:template name="declarationSpecifiers0">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='storageClassSpecifier'" >
          <xsl:call-template name="storageClassSpecifier" />
        </xsl:when>
        <xsl:when test="local-name()='declarationSpecifiers0'" >
          <xsl:call-template name="declarationSpecifiers0" />
        </xsl:when>
        <xsl:when test="local-name()='typeQualifier'" >
          <xsl:call-template name="typeQualifier" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                            storageClassSpecifier                    -->
  <!-- =================================================================== -->
  <xsl:template name="storageClassSpecifier">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='storageClassSpecifierTypedef'" >
          <xsl:call-template name="storageClassSpecifierTypedef" />
        </xsl:when>
        <xsl:when test="local-name()='EXTERN'" >
          <xsl:call-template name="EXTERN" />
        </xsl:when>
        <xsl:when test="local-name()='STATIC'" >
          <xsl:call-template name="STATIC" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                        storageClassSpecifierTypedef                 -->
  <!-- =================================================================== -->
  <xsl:template name="storageClassSpecifierTypedef">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='TYPEDEF'" >
          <xsl:call-template name="TYPEDEF" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                TYPEDEF                              -->
  <!-- =================================================================== -->
  <xsl:template name="TYPEDEF">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 EXTERN                              -->
  <!-- =================================================================== -->
  <xsl:template name="EXTERN">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 STATIC                              -->
  <!-- =================================================================== -->
  <xsl:template name="STATIC">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                typeQualifier                        -->
  <!-- =================================================================== -->
  <xsl:template name="typeQualifier">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='CONST'" >
          <xsl:call-template name="CONST" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                    CONST                            -->
  <!-- =================================================================== -->
  <xsl:template name="CONST">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                               typeSpecifier1                        -->
  <!-- =================================================================== -->
  <xsl:template name="typeSpecifier1">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='VOID'" >
          <xsl:call-template name="VOID" />
        </xsl:when>
        <xsl:when test="local-name()='FLOAT'" >
          <xsl:call-template name="FLOAT" />
        </xsl:when>
        <xsl:when test="local-name()='structOrUnionSpecifier'" >
          <xsl:call-template name="structOrUnionSpecifier" />
        </xsl:when>
        <xsl:when test="local-name()='enumSpecifier'" >
          <xsl:call-template name="enumSpecifier" />
        </xsl:when>
        <xsl:when test="local-name()='TYPEDEF_NAME'" >
          <xsl:call-template name="TYPEDEF_NAME" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                   VOID                              -->
  <!-- =================================================================== -->
  <xsl:template name="VOID">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  FLOAT                              -->
  <!-- =================================================================== -->
  <xsl:template name="FLOAT">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                             structOrUnionSpecifier                  -->
  <!-- =================================================================== -->
  <xsl:template name="structOrUnionSpecifier">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  enumSpecifier                      -->
  <!-- =================================================================== -->
  <xsl:template name="enumSpecifier">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 TYPEDEF_NAME                        -->
  <!-- =================================================================== -->
  <xsl:template name="TYPEDEF_NAME">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                               typeSpecifier2                        -->
  <!-- =================================================================== -->
  <xsl:template name="typeSpecifier2">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='CHAR'" >
          <xsl:call-template name="CHAR" />
        </xsl:when>
        <xsl:when test="local-name()='SHORT'" >
          <xsl:call-template name="SHORT" />
        </xsl:when>
        <xsl:when test="local-name()='INT'" >
          <xsl:call-template name="INT" />
        </xsl:when>
        <xsl:when test="local-name()='LONG'" >
          <xsl:call-template name="LONG" />
        </xsl:when>
        <xsl:when test="local-name()='DOUBLE'" >
          <xsl:call-template name="DOUBLE" />
        </xsl:when>
        <xsl:when test="local-name()='SIGNED'" >
          <xsl:call-template name="SIGNED" />
        </xsl:when>
        <xsl:when test="local-name()='UNSIGNED'" >
          <xsl:call-template name="UNSIGNED" />
        </xsl:when>
        <xsl:when test="local-name()='BOOL'" >
          <xsl:call-template name="BOOL" />
        </xsl:when>
        <xsl:when test="local-name()='COMPLEX'" >
          <xsl:call-template name="COMPLEX" />
        </xsl:when>
        <xsl:when test="local-name()='IMAGINARY'" >
          <xsl:call-template name="IMAGINARY" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                           declarationSpecifiers1                    -->
  <!-- =================================================================== -->
  <xsl:template name="declarationSpecifiers1">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='typeSpecifier1'" >
          <xsl:call-template name="typeSpecifier1" />
        </xsl:when>
        <xsl:when test="local-name()='declarationSpecifiers0'" >
          <xsl:call-template name="declarationSpecifiers0" />
        </xsl:when>
        <xsl:when test="local-name()='declarationSpecifiers1'" >
          <xsl:call-template name="declarationSpecifiers1" />
        </xsl:when>
        <xsl:when test="local-name()='storageClassSpecifier'" >
          <xsl:call-template name="storageClassSpecifier" />
        </xsl:when>
        <xsl:when test="local-name()='typeQualifier'" >
          <xsl:call-template name="typeQualifier" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                           declarationSpecifiers2                    -->
  <!-- =================================================================== -->
  <xsl:template name="declarationSpecifiers2">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='typeSpecifier2'" >
          <xsl:call-template name="typeSpecifier2" />
        </xsl:when>
        <xsl:when test="local-name()='declarationSpecifiers0'" >
          <xsl:call-template name="declarationSpecifiers0" />
        </xsl:when>
        <xsl:when test="local-name()='declarationSpecifiers2'" >
          <xsl:call-template name="declarationSpecifiers2" />
        </xsl:when>
        <xsl:when test="local-name()='storageClassSpecifier'" >
          <xsl:call-template name="storageClassSpecifier" />
        </xsl:when>
        <xsl:when test="local-name()='typeQualifier'" >
          <xsl:call-template name="typeQualifier" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                      declarationCheckdeclarationSpecifiers          -->
  <!-- =================================================================== -->
  <xsl:template name="declarationCheckdeclarationSpecifiers">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='declarationSpecifiers'" >
          <xsl:call-template name="declarationSpecifiers" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                        declarationCheckinitDeclaratorList           -->
  <!-- =================================================================== -->
  <xsl:template name="declarationCheckinitDeclaratorList">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='initDeclaratorList'" >
          <xsl:call-template name="initDeclaratorList" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                             initDeclaratorList                      -->
  <!-- =================================================================== -->
  <xsl:template name="initDeclaratorList">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                   CHAR                              -->
  <!-- =================================================================== -->
  <xsl:template name="CHAR">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  SHORT                              -->
  <!-- =================================================================== -->
  <xsl:template name="SHORT">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                   INT                               -->
  <!-- =================================================================== -->
  <xsl:template name="INT">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  LONG                               -->
  <!-- =================================================================== -->
  <xsl:template name="LONG">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 DOUBLE                              -->
  <!-- =================================================================== -->
  <xsl:template name="DOUBLE">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 SIGNED                              -->
  <!-- =================================================================== -->
  <xsl:template name="SIGNED">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                UNSIGNED                             -->
  <!-- =================================================================== -->
  <xsl:template name="UNSIGNED">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  BOOL                               -->
  <!-- =================================================================== -->
  <xsl:template name="BOOL">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 LABEL                               -->
  <!-- =================================================================== -->
  <xsl:template name="LABEL">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                COMPLEX                              -->
  <!-- =================================================================== -->
  <xsl:template name="COMPLEX">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                               IMAGINARY                             -->
  <!-- =================================================================== -->
  <xsl:template name="IMAGINARY">
    <xsl:variable name="dummyTracef" select="hsl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

</xsl:stylesheet>
