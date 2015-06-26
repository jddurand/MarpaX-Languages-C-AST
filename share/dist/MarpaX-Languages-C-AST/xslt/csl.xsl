<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:csl="urn:csl">
  <!-- <xsl:output method="text" omit-xml-declaration="yes" /> -->

  <!-- =================================================================== -->
  <!--                                  MAIN                               -->
  <!-- =================================================================== -->

  <xsl:template match="/">
    <!-- Select only declarations -->
    <cdecls>
      <xsl:for-each select="./translationUnit/externalDeclaration/declaration" >
        <cdecl>
          <xsl:call-template name="declaration" />
        </cdecl>
      </xsl:for-each>
    </cdecls>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                              declaration                            -->
  <!-- =================================================================== -->
  <xsl:template name="declaration">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <TYPEDEF/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 EXTERN                              -->
  <!-- =================================================================== -->
  <xsl:template name="EXTERN">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <EXTERN/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 STATIC                              -->
  <!-- =================================================================== -->
  <xsl:template name="STATIC">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <STATIC/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                typeQualifier                        -->
  <!-- =================================================================== -->
  <xsl:template name="typeQualifier">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <CONST/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                               typeSpecifier1                        -->
  <!-- =================================================================== -->
  <xsl:template name="typeSpecifier1">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <VOID/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  FLOAT                              -->
  <!-- =================================================================== -->
  <xsl:template name="FLOAT">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <FLOAT/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                             structOrUnionSpecifier                  -->
  <!-- =================================================================== -->
  <xsl:template name="structOrUnionSpecifier">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='structOrUnion'" >
          <xsl:call-template name="structOrUnion" />
        </xsl:when>
        <xsl:when test="local-name()='structDeclarationList'" >
          <LIST>
            <xsl:call-template name="structDeclarationList" />
          </LIST>
        </xsl:when>
        <xsl:when test="local-name()='IDENTIFIER_UNAMBIGUOUS'" >
          <xsl:call-template name="IDENTIFIER_UNAMBIGUOUS" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                              structOrUnion                          -->
  <!-- =================================================================== -->
  <xsl:template name="structOrUnion">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='STRUCT'" >
          <xsl:call-template name="STRUCT" />
        </xsl:when>
        <xsl:when test="local-name()='UNION'" >
          <xsl:call-template name="UNION" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                    STRUCT                           -->
  <!-- =================================================================== -->
  <xsl:template name="STRUCT">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <STRUCT/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                    UNION                            -->
  <!-- =================================================================== -->
  <xsl:template name="UNION">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <UNION/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                           structDeclarationList                     -->
  <!-- =================================================================== -->
  <xsl:template name="structDeclarationList">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <cdecls>
      <xsl:for-each select="./*" >
        <xsl:choose>
          <xsl:when test="local-name()='structDeclaration'" >
            <cdecl>
              <xsl:call-template name="structDeclaration" />
            </cdecl>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </cdecls>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                             structDeclaration                       -->
  <!-- =================================================================== -->
  <xsl:template name="structDeclaration">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='specifierQualifierList'" >
          <xsl:call-template name="specifierQualifierList" />
        </xsl:when>
        <xsl:when test="local-name()='structDeclaratorList'" >
          <xsl:call-template name="structDeclaratorList" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                          specifierQualifierList                     -->
  <!-- =================================================================== -->
  <xsl:template name="specifierQualifierList">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='specifierQualifierList0'" >
          <xsl:call-template name="specifierQualifierList0" />
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList1'" >
          <xsl:call-template name="specifierQualifierList1" />
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList2'" >
          <xsl:call-template name="specifierQualifierList2" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                          specifierQualifierList0                    -->
  <!-- =================================================================== -->
  <xsl:template name="specifierQualifierList0">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='typeQualifier'" >
          <xsl:call-template name="typeQualifier" />
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList0'" >
          <xsl:call-template name="specifierQualifierList0" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                          specifierQualifierList1                    -->
  <!-- =================================================================== -->
  <xsl:template name="specifierQualifierList1">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='typeSpecifier1'" >
          <xsl:call-template name="typeSpecifier1" />
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList0'" >
          <xsl:call-template name="specifierQualifierList0" />
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList1'" >
          <xsl:call-template name="specifierQualifierList1" />
        </xsl:when>
        <xsl:when test="local-name()='typeQualifier'" >
          <xsl:call-template name="typeQualifier" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                          specifierQualifierList2                    -->
  <!-- =================================================================== -->
  <xsl:template name="specifierQualifierList2">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='typeSpecifier2'" >
          <xsl:call-template name="typeSpecifier2" />
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList0'" >
          <xsl:call-template name="specifierQualifierList0" />
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList2'" >
          <xsl:call-template name="specifierQualifierList2" />
        </xsl:when>
        <xsl:when test="local-name()='typeQualifier'" >
          <xsl:call-template name="typeQualifier" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                            structDeclaratorList                     -->
  <!-- =================================================================== -->
  <xsl:template name="structDeclaratorList">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='structDeclarator'" >
          <xsl:call-template name="structDeclarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                              structDeclarator                       -->
  <!-- =================================================================== -->
  <xsl:template name="structDeclarator">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='declarator'" >
          <xsl:call-template name="declarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 declarator                          -->
  <!-- =================================================================== -->
  <xsl:template name="declarator">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='pointer'" >
          <xsl:call-template name="pointer" />
        </xsl:when>
        <xsl:when test="local-name()='directDeclarator'" >
          <xsl:call-template name="directDeclarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                             directDeclarator                        -->
  <!-- =================================================================== -->
  <xsl:template name="directDeclarator">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='directDeclaratorIdentifier'" >
          <xsl:call-template name="directDeclaratorIdentifier" />
        </xsl:when>
        <xsl:when test="local-name()='declarator'" >
          <xsl:call-template name="declarator" />
        </xsl:when>
        <xsl:when test="local-name()='directDeclarator'" >
          <xsl:call-template name="directDeclarator" />
        </xsl:when>
        <xsl:when test="local-name()='LBRACKET'" >
          <xsl:call-template name="LBRACKET" />
        </xsl:when>
        <xsl:when test="local-name()='RBRACKET'" >
          <xsl:call-template name="RBRACKET" />
        </xsl:when>
        <xsl:when test="local-name()='LPAREN_SCOPE'" >
          <xsl:call-template name="LPAREN_SCOPE" />
        </xsl:when>
        <xsl:when test="local-name()='RPAREN_SCOPE'" >
          <xsl:call-template name="RPAREN_SCOPE" />
        </xsl:when>
        <xsl:when test="local-name()='parameterTypeList'" >
          <LIST>
            <xsl:call-template name="parameterTypeList" />
          </LIST>
        </xsl:when>
        <xsl:when test="local-name()='identifierList'" >
          <xsl:call-template name="identifierList" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                          directDeclaratorIdentifier                 -->
  <!-- =================================================================== -->
  <xsl:template name="directDeclaratorIdentifier">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='IDENTIFIER'" >
          <xsl:call-template name="IDENTIFIER" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                            parameterTypeList                        -->
  <!-- =================================================================== -->
  <xsl:template name="parameterTypeList">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <cdecls>
      <xsl:for-each select="./*" >
        <xsl:choose>
          <xsl:when test="local-name()='parameterList'" >
            <cdecl>
              <xsl:call-template name="parameterList" />
            </cdecl>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </cdecls>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                              parameterList                          -->
  <!-- =================================================================== -->
  <xsl:template name="parameterList">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='parameterDeclaration'" >
          <xsl:call-template name="parameterDeclaration" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                            parameterDeclaration                     -->
  <!-- =================================================================== -->
  <xsl:template name="parameterDeclaration">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='parameterDeclarationCheck'" >
          <xsl:call-template name="parameterDeclarationCheck" />
        </xsl:when>
        <xsl:when test="local-name()='declarationSpecifiers'" >
          <xsl:call-template name="declarationSpecifiers" />
        </xsl:when>
        <xsl:when test="local-name()='abstractDeclarator'" >
          <xsl:call-template name="abstractDeclarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                          parameterDeclarationCheck                  -->
  <!-- =================================================================== -->
  <xsl:template name="parameterDeclarationCheck">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='parameterDeclarationdeclarationSpecifiers'" >
          <xsl:call-template name="parameterDeclarationdeclarationSpecifiers" />
        </xsl:when>
        <xsl:when test="local-name()='parameterDeclarationCheckDeclarator'" >
          <xsl:call-template name="parameterDeclarationCheckDeclarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                 parameterDeclarationCheckDeclarator                 -->
  <!-- =================================================================== -->
  <xsl:template name="parameterDeclarationCheckDeclarator">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='declarator'" >
          <xsl:call-template name="declarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--            parameterDeclarationdeclarationSpecifiers                -->
  <!-- =================================================================== -->
  <xsl:template name="parameterDeclarationdeclarationSpecifiers">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='declarationSpecifiers'" >
          <xsl:call-template name="declarationSpecifiers" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                              identifierList                         -->
  <!-- =================================================================== -->
  <xsl:template name="identifierList">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='IDENTIFIER'" >
          <xsl:call-template name="IDENTIFIER" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                              abstractDeclarator                     -->
  <!-- =================================================================== -->
  <xsl:template name="abstractDeclarator">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='pointer'" >
          <xsl:call-template name="pointer" />
        </xsl:when>
        <xsl:when test="local-name()='directAbstractDeclarator'" >
          <xsl:call-template name="directAbstractDeclarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                            directAbstractDeclarator                 -->
  <!-- =================================================================== -->
  <xsl:template name="directAbstractDeclarator">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='abstractDeclarator'" >
          <xsl:call-template name="abstractDeclarator" />
        </xsl:when>
        <xsl:when test="local-name()='LPAREN'" >
          <xsl:call-template name="LPAREN" />
        </xsl:when>
        <xsl:when test="local-name()='RPAREN'" >
          <xsl:call-template name="RPAREN" />
        </xsl:when>
        <xsl:when test="local-name()='LBRACKET'" >
          <xsl:call-template name="LBRACKET" />
        </xsl:when>
        <xsl:when test="local-name()='RBRACKET'" >
          <xsl:call-template name="RBRACKET" />
        </xsl:when>
        <xsl:when test="local-name()='directAbstractDeclarator'" >
          <xsl:call-template name="directAbstractDeclarator" />
        </xsl:when>
        <xsl:when test="local-name()='LPAREN_SCOPE'" >
          <xsl:call-template name="LPAREN_SCOPE" />
        </xsl:when>
        <xsl:when test="local-name()='RPAREN_SCOPE'" >
          <xsl:call-template name="RPAREN_SCOPE" />
        </xsl:when>
        <xsl:when test="local-name()='parameterTypeList'" >
          <xsl:call-template name="parameterTypeList" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  pointer                            -->
  <!-- =================================================================== -->
  <xsl:template name="pointer">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='STAR'" >
          <xsl:call-template name="STAR" />
        </xsl:when>
        <xsl:when test="local-name()='pointerQualifierList'" >
          <xsl:call-template name="pointerQualifierList" />
        </xsl:when>
        <xsl:when test="local-name()='pointer'" >
          <xsl:call-template name="pointer" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                         pointerQualifierList                        -->
  <!-- =================================================================== -->
  <xsl:template name="pointerQualifierList">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='pointerQualifier'" >
          <xsl:call-template name="pointerQualifier" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                           pointerQualifier                          -->
  <!-- =================================================================== -->
  <xsl:template name="pointerQualifier">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='typeQualifier'" >
          <xsl:call-template name="typeQualifier" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  enumSpecifier                      -->
  <!-- =================================================================== -->
  <xsl:template name="enumSpecifier">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='ENUM'" >
          <xsl:call-template name="ENUM" />
        </xsl:when>
        <xsl:when test="local-name()='enumeratorList'" >
          <LIST>
            <xsl:call-template name="enumeratorList" />
          </LIST>
        </xsl:when>
        <xsl:when test="local-name()='IDENTIFIER_UNAMBIGUOUS'" >
          <xsl:call-template name="IDENTIFIER_UNAMBIGUOUS" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 enumeratorList                      -->
  <!-- =================================================================== -->
  <xsl:template name="enumeratorList">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='enumerator'" >
          <xsl:call-template name="enumerator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                   enumerator                        -->
  <!-- =================================================================== -->
  <xsl:template name="enumerator">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='enumerationConstant'" >
          <xsl:call-template name="enumerationConstant" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                enumerationConstant                  -->
  <!-- =================================================================== -->
  <xsl:template name="enumerationConstant">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='enumerationConstantIdentifier'" >
          <xsl:call-template name="enumerationConstantIdentifier" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                           enumerationConstantIdentifier             -->
  <!-- =================================================================== -->
  <xsl:template name="enumerationConstantIdentifier">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='IDENTIFIER_UNAMBIGUOUS'" >
          <xsl:call-template name="IDENTIFIER_UNAMBIGUOUS" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                             IDENTIFIER_UNAMBIGUOUS                  -->
  <!-- =================================================================== -->
  <xsl:template name="IDENTIFIER_UNAMBIGUOUS">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:text disable-output-escaping="yes">&lt;</xsl:text>IDENTIFIER name="<xsl:value-of select="./@text" />"<xsl:text disable-output-escaping="yes">&#47;&gt;</xsl:text>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  IDENTIFIER                         -->
  <!-- =================================================================== -->
  <xsl:template name="IDENTIFIER">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:text disable-output-escaping="yes">&lt;</xsl:text>IDENTIFIER name="<xsl:value-of select="./@text" />"<xsl:text disable-output-escaping="yes">&#47;&gt;</xsl:text>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  LBRACKET                           -->
  <!-- =================================================================== -->
  <xsl:template name="LBRACKET">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:text disable-output-escaping="yes">&lt;</xsl:text>ARRAY<xsl:text disable-output-escaping="yes">&gt;</xsl:text>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  RBRACKET                           -->
  <!-- =================================================================== -->
  <xsl:template name="RBRACKET">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:text disable-output-escaping="yes">&lt;&#47;</xsl:text>ARRAY<xsl:text disable-output-escaping="yes">&gt;</xsl:text>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                LPAREN_SCOPE                         -->
  <!-- =================================================================== -->
  <xsl:template name="LPAREN_SCOPE">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                RPAREN_SCOPE                         -->
  <!-- =================================================================== -->
  <xsl:template name="RPAREN_SCOPE">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  LPAREN                             -->
  <!-- =================================================================== -->
  <xsl:template name="LPAREN">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 RPAREN                              -->
  <!-- =================================================================== -->
  <xsl:template name="RPAREN">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                     STAR                            -->
  <!-- =================================================================== -->
  <xsl:template name="STAR">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <STAR/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 TYPEDEF_NAME                        -->
  <!-- =================================================================== -->
  <xsl:template name="TYPEDEF_NAME">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:text disable-output-escaping="yes">&lt;</xsl:text>TYPEDEF_NAME name="<xsl:value-of select="./@text" />"<xsl:text disable-output-escaping="yes">&#47;&gt;</xsl:text>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                     ENUM                            -->
  <!-- =================================================================== -->
  <xsl:template name="ENUM">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <ENUM/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                               typeSpecifier2                        -->
  <!-- =================================================================== -->
  <xsl:template name="typeSpecifier2">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
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
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='initDeclarator'" >
          <xsl:call-template name="initDeclarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                               initDeclarator                        -->
  <!-- =================================================================== -->
  <xsl:template name="initDeclarator">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <xsl:for-each select="./*" >
      <xsl:choose>
        <xsl:when test="local-name()='declarator'" >
          <xsl:call-template name="declarator" />
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                   CHAR                              -->
  <!-- =================================================================== -->
  <xsl:template name="CHAR">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <CHAR/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  SHORT                              -->
  <!-- =================================================================== -->
  <xsl:template name="SHORT">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <SHORT/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                   INT                               -->
  <!-- =================================================================== -->
  <xsl:template name="INT">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <INT/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  LONG                               -->
  <!-- =================================================================== -->
  <xsl:template name="LONG">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <LONG/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 DOUBLE                              -->
  <!-- =================================================================== -->
  <xsl:template name="DOUBLE">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <DOUBLE/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 SIGNED                              -->
  <!-- =================================================================== -->
  <xsl:template name="SIGNED">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <SIGNED/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                UNSIGNED                             -->
  <!-- =================================================================== -->
  <xsl:template name="UNSIGNED">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <UNSIGNED/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                  BOOL                               -->
  <!-- =================================================================== -->
  <xsl:template name="BOOL">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <BOOL/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                 LABEL                               -->
  <!-- =================================================================== -->
  <xsl:template name="LABEL">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <LABEL/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                                COMPLEX                              -->
  <!-- =================================================================== -->
  <xsl:template name="COMPLEX">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <COMPLEX/>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                               IMAGINARY                             -->
  <!-- =================================================================== -->
  <xsl:template name="IMAGINARY">
    <xsl:variable name="dummyTracef" select="csl:tracef('%s: %s', local-name(), ./@text)" />
    <IMAGINARY/>
  </xsl:template>

</xsl:stylesheet>
