<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:hsl="urn:hsl">
  <xsl:output method="text" omit-xml-declaration="yes" />

  <!-- =================================================================== -->
  <!--                            MAIN                                     -->
  <!-- =================================================================== -->

  <xsl:template match="/">
    <xsl:variable name="content"   select="hsl:content()" />
    <xsl:variable name="input"     select="hsl:input()" />
    <xsl:variable name="module"    select="hsl:module()" />
    <xsl:variable name="localtime" select="hsl:localtime()" />
    <xsl:variable name="prefix"    select="hsl:prefix()" />
/* =================================================== */
/* INPUT                                               */
/* =================================================== */
<xsl:value-of select="$content"/>
/* =================================================== */
/* DECLARATIONS                                        */
/* =================================================== */
    <xsl:variable name="resetAnonCounterDecl" select="hsl:resetAnonCounter()" />
    <xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
      <xsl:call-template name="topStructOrUnionSpecifier">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="mode" select="'decl'"/>
        <xsl:with-param name="prefix" select="$prefix"/>
      </xsl:call-template>
    </xsl:for-each>
/* =================================================== */
/* DEFINITIONS                                         */
/* =================================================== */
    <xsl:variable name="resetAnonCounterDef" select="hsl:resetAnonCounter()" />
    <xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
      <xsl:call-template name="topStructOrUnionSpecifier">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="mode" select="'def'"/>
        <xsl:with-param name="prefix" select="$prefix"/>
      </xsl:call-template>
    </xsl:for-each>
/* =================================================== */
/* INTERFACE                                           */
/* =================================================== */
    <xsl:variable name="resetAnonCounterIfce" select="hsl:resetAnonCounter()" />
    <xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
      <xsl:call-template name="topStructOrUnionSpecifier">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="mode" select="'ifce'"/>
        <xsl:with-param name="prefix" select="$prefix"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <!-- =================================================================== -->
  <!--               . = Top Level Structure Or Union                      -->
  <!-- =================================================================== -->

  <xsl:template name="topStructOrUnionSpecifier">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="prefix" />
    <!-- Decypher the structure or union -->
    <xsl:call-template name="structOrUnionSpecifier">
      <xsl:with-param name="top" select="1"/>
      <xsl:with-param name="module" select="$module"/>
      <xsl:with-param name="mode" select="$mode"/>
      <xsl:with-param name="prefix" select="$prefix"/>
    </xsl:call-template>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                   structOrUnionSpecifier                            -->
  <!-- =================================================================== -->

  <xsl:template name="structOrUnionSpecifier">
    <xsl:param name="top" />
    <xsl:param name="module" />
    <xsl:param name="prefix" />
    <xsl:param name="mode" />
    <!-- Get the identifier, eventually anonymous -->
    <xsl:variable name="identifierUnambiguous" select="./IDENTIFIER_UNAMBIGUOUS" />
    <xsl:variable name="content" select="./structDeclarationList/@text" />
    <xsl:variable name="identifier">
      <xsl:choose>
        <xsl:when test="$identifierUnambiguous">
          <xsl:value-of select="$identifierUnambiguous/@text" />
        </xsl:when>
        <xsl:otherwise>
          <!-- We do our best to not repeat generated anonymous identifiers -->
          <xsl:if test="hsl:getContentToIdentifier($content)!=''">
            <xsl:value-of select="hsl:getContentToIdentifier($content)" />
          </xsl:if>
          <xsl:if test="hsl:getContentToIdentifier($content)=''">
            <xsl:value-of select="concat(hsl:anon(), hsl:nextAnonCounter())"/>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!-- If first time for this anonymous, do a declaration -->
    <xsl:if test="($mode='decl') and (not($identifierUnambiguous)) and (hsl:getContentToIdentifier($content)='')">
typedef <xsl:value-of select="./structOrUnion/@text" /> {
  <xsl:value-of select="$content" />
} <xsl:value-of select="$identifier"/>;
      <xsl:variable name="dummySetContentToIdentifier" select="hsl:setContentToIdentifier($content, $identifier)"/>
    </xsl:if>
    <xsl:if test="hsl:getDoneIdentifierPerMode($mode, $identifier)=''">
      <xsl:if test="($top='1')">
        <xsl:choose>
          <!--  Structure Or Union: Declarations   -->
          <xsl:when test="$mode = 'decl'">
[% DECL_CONSTRUCTOR identifier='<xsl:value-of select="$identifier"/>' %]
[% DECL_DESTRUCTOR identifier='<xsl:value-of select="$identifier"/>' %]
          </xsl:when>
          <!--  Structure Or Union: Definitions    -->
          <xsl:when test="$mode = 'def'">
[% DEF_CONSTRUCTOR identifier='<xsl:value-of select="$identifier"/>' %]
[% DEF_DESTRUCTOR identifier='<xsl:value-of select="$identifier"/>' %]
          </xsl:when>
          <!--  Structure Or Union: Interface    -->
          <xsl:when test="$mode = 'ifce'">
[% IFCE module='<xsl:value-of select="$module"/>' package='<xsl:value-of select="$identifier"/>' %]
[% IFCE_CONSTRUCTOR identifier='<xsl:value-of select="$identifier"/>' %]
[% IFCE_DESTRUCTOR identifier='<xsl:value-of select="$identifier"/>' %]
          </xsl:when>
        </xsl:choose>
      </xsl:if>
      <xsl:for-each select="./structDeclarationList/*/structDeclaratorList/*/declarator">
        <xsl:call-template name="declarator">
          <xsl:with-param name="module" select="$module"/>
          <xsl:with-param name="mode" select="$mode" />
          <xsl:with-param name="identifier" select="$identifier" />
        </xsl:call-template>
      </xsl:for-each>
      <xsl:variable name="dummySetDoneIdentifierPerMode" select="hsl:setDoneIdentifierPerMode($mode,$identifier)" />
    </xsl:if>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                       Decypher declarators                          -->
  <!-- =================================================================== -->

  <xsl:template name="declarator">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <!-- by definition the first found identifier is the one we are looking for -->
    <xsl:variable name="IDENTIFIER" select=".//IDENTIFIER[1]" />
    <xsl:variable name="member" select="$IDENTIFIER/@text"/>
    <xsl:choose>
      <xsl:when test="$mode = 'decl'">
[% DECL_ACCESSOR identifier='<xsl:value-of select="$identifier"/>' member='<xsl:value-of select="$member"/>' %]
      </xsl:when>
      <xsl:when test="$mode = 'def'">
[% DEF_ACCESSOR identifier='<xsl:value-of select="$identifier"/>' member='<xsl:value-of select="$member"/>' %]
      </xsl:when>
      <xsl:when test="$mode = 'ifce'">
[% IFCE_ACCESSOR identifier='<xsl:value-of select="$identifier"/>' member='<xsl:value-of select="$member"/>' %]
      </xsl:when>
    </xsl:choose>
    <!-- Decypher the declarator -->
    <xsl:for-each select="$IDENTIFIER/../..">
      <xsl:call-template name="decypherDirectDeclarator">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="identifier" select="$identifier" />
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherDirectDeclarator">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <!-- Look for the next lexeme on the right -->
    <xsl:variable name="nextLexeme" select="./following-sibling::*[1]" />
    <xsl:choose>
      <xsl:when test="$nextLexeme[local-name()='LBRACKET']">
        <xsl:if test="$mode = 'ifce'">
          <!-- We take all the text as-is up to the RBRACKET -->
[% IFCE_DECYPHER_ARRAY %]
            <!-- If you want the size, you could do: <xsl:for-each select="$nextLexeme/following-sibling::*[local-name()!='RBRACKET']"><xsl:value-of select="concat(' ', ./@text)" /></xsl:for-each> -->
        </xsl:if>
      </xsl:when>
      <xsl:when test="$nextLexeme[local-name()='LPAREN_SCOPE']">
        <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_FUNCTION %]
        </xsl:if>
      </xsl:when>
    </xsl:choose>
    <!--
        Look to parent of directDeclarator. This can only be:
        * declarator
        * directDeclarator
    -->
    <xsl:for-each select="..">
      <xsl:choose>
        <xsl:when test="local-name()='declarator'">
          <xsl:call-template name="decypherDeclarator">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='directDeclarator'">
          <xsl:call-template name="decypherDirectDeclarator">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherPointer">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <!--
        A pointer may have a pointerQualifierList that is nothing else but
        a typeQualifier, or again a pointer. Nevertheless there is the
        subtility of CONST lexeme. CONST belong only in typeQualifier, and the
        the rule is: if there is CONST in pointerQualifier, it means the pointer
        is read-only.
    -->
    <xsl:if test="./pointerQualifierList/pointerQualifier/typeQualifier/CONST">
      <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_READONLY %]
      </xsl:if>
    </xsl:if>
    <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_POINTER %]
    </xsl:if>
    <!-- Then take all typeQualifiers except CONST -->
    <xsl:for-each select="./pointerQualifierList/pointerQualifier/typeQualifier">
      <xsl:call-template name="decypherTypeQualifier">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="withConst" select="0" />
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="identifier" select="$identifier" />
      </xsl:call-template>
    </xsl:for-each>
    <xsl:for-each select="./pointer">
      <xsl:call-template name="decypherPointer">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="identifier" select="$identifier" />
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherDeclarator">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <!--
        The only important thing is to know if there is a pointer, which can
        only be the very first child of declarator
    -->
    <xsl:for-each select="./child::*[1][local-name()='pointer']">
      <xsl:call-template name="decypherPointer">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="identifier" select="$identifier" />
      </xsl:call-template>
    </xsl:for-each>
    <!--
        Look to parent of declarator. This can only be:
        * initDeclarator                       : cannot happen in a context of a structure declaration
        * structDeclarator                     : we are interested in specifierQualifierList
        * directDeclarator
        * parameterDeclarationCheckDeclarator  : we are not in the context of a parameter declaration
        * fileScopeDeclarator                  : end of the story
    -->
    <xsl:for-each select="..">
      <xsl:choose>
        <xsl:when test="local-name()='structDeclarator'">
          <!-- structDeclaratorList/structDeclarator -->
          <xsl:for-each select="../preceding-sibling::*[1]">
            <xsl:call-template name="decypherSpecifierQualifierList">
              <xsl:with-param name="module" select="$module"/>
              <xsl:with-param name="mode" select="$mode" />
              <xsl:with-param name="identifier" select="$identifier" />
            </xsl:call-template>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="local-name()='directDeclarator'">
          <xsl:call-template name="decypherDirectDeclarator">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherSpecifierQualifierList">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <xsl:for-each select="./*">
      <xsl:choose>
        <xsl:when test="local-name()='specifierQualifierList0'">
          <xsl:call-template name="decypherSpecifierQualifierList0">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList1'">
          <xsl:call-template name="decypherSpecifierQualifierList1">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList2'">
          <xsl:call-template name="decypherSpecifierQualifierList2">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherSpecifierQualifierList0">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <xsl:for-each select="./*">
      <xsl:choose>
        <xsl:when test="local-name()='typeQualifier'">
          <xsl:call-template name="decypherTypeQualifier">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="withConst" select="1" />
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList0'">
          <xsl:call-template name="decypherSpecifierQualifierList0">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherSpecifierQualifierList1">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <xsl:for-each select="./*">
      <xsl:choose>
        <xsl:when test="local-name()='typeSpecifier1'">
          <xsl:call-template name="decypherTypeSpecifier1">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='typeQualifier'">
          <xsl:call-template name="decypherTypeQualifier">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="withConst" select="1" />
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList0'">
          <xsl:call-template name="decypherSpecifierQualifierList0">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList1'">
          <xsl:call-template name="decypherSpecifierQualifierList1">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherSpecifierQualifierList2">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <xsl:for-each select="./*">
      <xsl:choose>
        <xsl:when test="local-name()='typeSpecifier2'">
          <xsl:call-template name="decypherTypeSpecifier2">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='typeQualifier'">
          <xsl:call-template name="decypherTypeQualifier">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="withConst" select="1" />
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList0'">
          <xsl:call-template name="decypherSpecifierQualifierList0">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="local-name()='specifierQualifierList2'">
          <xsl:call-template name="decypherSpecifierQualifierList2">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherTypeSpecifier1">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <xsl:for-each select="./*">
      <xsl:choose>
        <xsl:when test="local-name()='VOID'">
          <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_TYPE_SPECIFIER type='void' %]
          </xsl:if>
        </xsl:when>
        <xsl:when test="local-name()='FLOAT'">
          <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_TYPE_SPECIFIER type='float' %]
          </xsl:if>
        </xsl:when>
        <xsl:when test="local-name()='structOrUnionSpecifier'">
          <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_TYPE_INNER_STRUCTORUNION_START %]
          </xsl:if>
          <!-- Decypher the structure or union -->
          <xsl:call-template name="structOrUnionSpecifier">
            <xsl:with-param name="top" select="0"/>
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="identifier" select="$identifier"/>
            <xsl:with-param name="mode" select="$mode"/>
          </xsl:call-template>
          <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_TYPE_INNER_STRUCTORUNION_END %]
          </xsl:if>
        </xsl:when>
        <xsl:when test="local-name()='TYPEDEF_NAME'">
          <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_TYPE_SPECIFIER type='<xsl:value-of select="./@text" />' %]
          </xsl:if>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherTypeQualifier">
    <xsl:param name="module" />
    <xsl:param name="withConst" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <xsl:for-each select="./*">
      <xsl:if test="($withConst='1' and local-name()='CONST') or ($withConst='0' and local-name()!='CONST')" >
        <xsl:if test="local-name()='CONST'">
          <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_READONLY %]
          </xsl:if>
        </xsl:if>
        <xsl:if test="local-name()!='CONST'">
          <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_TYPE_QUALIFIER type='<xsl:value-of select="./@text" />' %]
          </xsl:if>
        </xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="decypherTypeSpecifier2">
    <xsl:param name="module" />
    <xsl:param name="mode" />
    <xsl:param name="identifier" />
    <xsl:for-each select="./*">
      <xsl:if test="$mode = 'ifce'">
[% IFCE_DECYPHER_TYPE_SPECIFIER type='<xsl:value-of select="./@text" />' %]
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>

