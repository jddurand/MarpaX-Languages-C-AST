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
/* ---------------------------------------------------- */
<xsl:value-of select="$content"/>
    <xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
      <xsl:call-template name="topStructOrUnionSpecifier">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="topStructOrUnionSpecifierCounter" select="position()"/>
        <xsl:with-param name="mode" select="'decl'"/>
        <xsl:with-param name="prefix" select="$prefix"/>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
      <xsl:call-template name="topStructOrUnionSpecifier">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="topStructOrUnionSpecifierCounter" select="position()"/>
        <xsl:with-param name="mode" select="'def'"/>
        <xsl:with-param name="prefix" select="$prefix"/>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
      <xsl:call-template name="topStructOrUnionSpecifier">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="topStructOrUnionSpecifierCounter" select="position()"/>
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
    <xsl:param name="topStructOrUnionSpecifierCounter" />
    <xsl:param name="mode" />
    <xsl:param name="prefix" />
    <!-- Get the identifier, eventually anonymous -->
    <xsl:variable name="identifierUnambiguous" select="./IDENTIFIER_UNAMBIGUOUS" />
    <xsl:variable name="identifier">
      <xsl:choose>
        <xsl:when test="$identifierUnambiguous">
          <xsl:value-of select="$identifierUnambiguous/@text" />
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat('__ANON__', $topStructOrUnionSpecifierCounter)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!-- If anonymous, do a declaration -->
    <xsl:if test="(($mode = 'decl') and not($identifierUnambiguous))">
      <!-- Get the type (struct or union) -->
      <xsl:variable name="structOrUnion" select="./structOrUnion/@text" />
      <!-- Get the declaration <li></li>st, cannot be empty when there is no identifier -->
      <xsl:variable name="structDeclarationList" select="./structDeclarationList/@text" />
typedef <xsl:value-of select="$structOrUnion" /> {
  <xsl:value-of select="$structDeclarationList" />
} <xsl:value-of select="$identifier"/>;
    </xsl:if>
    <!-- Decypher the structure or union -->
    <xsl:call-template name="structOrUnionSpecifier">
      <xsl:with-param name="module" select="$module"/>
      <xsl:with-param name="identifier" select="$identifier"/>
      <xsl:with-param name="mode" select="$mode"/>
      <xsl:with-param name="prefix" select="$prefix"/>
    </xsl:call-template>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                   structOrUnionSpecifier                            -->
  <!-- =================================================================== -->

  <xsl:template name="structOrUnionSpecifier">
    <xsl:param name="prefix" />
    <xsl:param name="module" />
    <xsl:param name="identifier" />
    <xsl:param name="mode" />
/* ---------------------------------------------------- */
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
        <!-- Accessors -->
      </xsl:when>
    </xsl:choose>
    <xsl:for-each select="./structDeclarationList/*/structDeclaratorList/*/declarator">
      <!-- In addition we are absolutely NOT interested by the type of what
           we return: we always return the address of the element. Full point. -->
      <xsl:call-template name="declarator">
        <xsl:with-param name="module" select="$module"/>
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="identifier" select="$identifier" />
      </xsl:call-template>
    </xsl:for-each>
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
    <xsl:variable name="declarator" select="$IDENTIFIER/@text"/>
    <xsl:choose>
      <xsl:when test="$mode = 'decl'">
[% DECL_ACCESSOR identifier='<xsl:value-of select="$identifier"/>' mode='rw' declarator='<xsl:value-of select="$declarator"/>' %]
      </xsl:when>
      <xsl:when test="$mode = 'def'">
[% DEF_ACCESSOR identifier='<xsl:value-of select="$identifier"/>' mode='rw' declarator='<xsl:value-of select="$declarator"/>' %]
      </xsl:when>
      <xsl:when test="$mode = 'ifce'">
[% IFCE_ACCESSOR identifier='<xsl:value-of select="$identifier"/>' mode='rw' declarator='<xsl:value-of select="$declarator"/>' %]
        <xsl:for-each select="$IDENTIFIER/../..">
          <!-- We decypther declarator only in the ifce case. This loop is just to have directDeclarator in "." -->
          <xsl:call-template name="decypherDirectDeclarator">
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="identifier" select="$identifier" />
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="decypherDirectDeclarator">
    <xsl:param name="identifier" />
    <!-- Look for the next lexeme on the right -->
    <xsl:variable name="nextLexeme" select="./following-sibling::*[1]" />
    <xsl:choose>
      <xsl:when test="$nextLexeme[local-name()='RPAREN']">
        RPAREN DETECTED
      </xsl:when>
      <xsl:when test="$nextLexeme[local-name()='LBRACKET']">
        /* Array */
        LBRACKET DETECTED
      </xsl:when>
      <xsl:when test="$nextLexeme[local-name()='LPAREN_SCOPE']">
        /* Function */
        LPAREN_SCOPE DETECTED
      </xsl:when>
      <xsl:otherwise>
        NO LEXEME ON THE RIGHT
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

