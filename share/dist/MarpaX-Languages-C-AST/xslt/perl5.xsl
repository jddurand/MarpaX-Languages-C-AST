<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:hsl="urn:hsl">
  <xsl:output method="text" omit-xml-declaration="yes" />

  <!-- =================================================================== -->
  <!--                            MAIN                                     -->
  <!-- =================================================================== -->

  <xsl:template match="/">
/* ==================================================== */
/*                 INTERFACE START                      */
/* ==================================================== */
/*
 * C binding pseudo code of <xsl:value-of select="hsl:opt('module')"/>
 * Generated <xsl:value-of select="hsl:localtime()"/>
 */

#define PERL_NO_GET_CONTEXT 1
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

/* ==================================================== */
/*                    USER INPUT                        */
/* ==================================================== */
<!-- left-aligned voluntarirly -->
<xsl:value-of select="hsl:content()"/>

/* ==================================================== */
/*               INTERNAL STRUCTURES                    */
/* ==================================================== */
typedef struct hsl_outer {
  short allocated; /* Heuristic tentative to protect against userland error */
} hsl_outer;

   <xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
     <!-- Declarations first -->
     <xsl:call-template name="topStructOrUnionSpecifier">
       <xsl:with-param name="topStructOrUnionSpecifierCounter" select="position()"/>
       <xsl:with-param name="mode" select="'decl'"/>
     </xsl:call-template>
   </xsl:for-each>

/* ==================================================== */
/*                        XSUB                          */
/* ==================================================== */
   <xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
     <!-- Then definitions -->
     <xsl:call-template name="topStructOrUnionSpecifier">
       <xsl:with-param name="topStructOrUnionSpecifierCounter" select="position()"/>
       <xsl:with-param name="mode" select="'def'"/>
     </xsl:call-template>
   </xsl:for-each>

/* ==================================================== */
/*                  INTERFACE END                       */
/* ==================================================== */
  </xsl:template>

  <!-- =================================================================== -->
  <!--                      Top Level Structure                            -->
  <!-- =================================================================== -->

  <xsl:template name="topStructOrUnionSpecifier">
    <xsl:param name="topStructOrUnionSpecifierCounter" />
    <xsl:param name="mode" />
    <!-- Get the identifier, eventually anonymous -->
    <xsl:variable name="identifier">
      <xsl:choose>
        <xsl:when test="./*[local-name()='IDENTIFIER_UNAMBIGUOUS']">
          <xsl:value-of select="./*[local-name()='IDENTIFIER_UNAMBIGUOUS']/@text"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat('__ANON__', $topStructOrUnionSpecifierCounter)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!-- Decypher the structure or union -->
    <xsl:call-template name="structOrUnionSpecifier">
      <xsl:with-param name="identifier" select="$identifier"/>
      <xsl:with-param name="mode" select="$mode"/>
    </xsl:call-template>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                   structOrUnionSpecifier                            -->
  <!-- =================================================================== -->

  <xsl:template name="structOrUnionSpecifier">
    <xsl:param name="identifier" />
    <xsl:param name="mode" />
    <xsl:variable name="hslIdentifier" select="concat(hsl:prefix(), $identifier)"/>
/* ---------------------------------------------------- */
    <xsl:choose>
      <!--  Structure Or Union: Declarations   -->
      <xsl:when test="$mode = 'decl'">
typedef struct <xsl:value-of select="$hslIdentifier"/> {
  struct <xsl:value-of select="$identifier"/> inner;
  hsl_outer outer;
} <xsl:value-of select="$hslIdentifier"/>;
      </xsl:when>
      <!--  Structure Or Union: Definitions    -->
      <xsl:when test="$mode = 'def'">
MODULE = <xsl:value-of select="hsl:opt('module')"/>	PACKAGE = <xsl:value-of select="concat(hsl:opt('module'), '::', $identifier)"/>

PROTOTYPES: ENABLE
        <!-- Generate creator, destructor -->
void *
<xsl:value-of select="$hslIdentifier"/>_new(pTHX_ char *CLASS)
PREINIT:
  void *rc;
CODE:
  Newx(rc, 1, <xsl:value-of select="$hslIdentifier"/>);
  RETVAL = rc;
OUTPUT: 
  RETVAL 

void 
DESTROY(pTHX_ void *self)
CODE: 
  Safefree(self); 
        <!-- Generate accessors -->
        <!-- we are only interested by declarations that declare something -->
        <xsl:for-each select="./structDeclarationList/*/structDeclaratorList/*/declarator">
          <!-- In addition we are absolutely NOT interested by the type of what
               we return: we always return the address of the element. Full point. -->
          <xsl:call-template name="declarator">
            <xsl:with-param name="hslIdentifier" select="$hslIdentifier" />
          </xsl:call-template>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                       Decypher declarators                          -->
  <!-- =================================================================== -->

  <xsl:template name="declarator">
    <xsl:param name="hslIdentifier" />
    <!-- by definition the first found identifier is the one we are looking for -->
    <xsl:variable name="IDENTIFIER" select=".//IDENTIFIER[1]" />
void *
<xsl:value-of select="$IDENTIFIER/@text"/>_get(pTHX_ void *self)
PREINIT:
  void *rc;
CODE:
  rc = &amp;(((<xsl:value-of select="$hslIdentifier"/> *) self)-><xsl:value-of select="$IDENTIFIER/@text"/>);
  { /* Decypher of <xsl:value-of select="./@text" /> */
  <xsl:for-each select="$IDENTIFIER/../..">
    <!-- Looping just to have directDeclarator in "." -->
    <xsl:call-template name="decypherDirectDeclarator">
      <xsl:with-param name="hslIdentifier" select="$hslIdentifier" />
    </xsl:call-template>
  </xsl:for-each>
  }
  RETVAL = rc;
OUTPUT:
  RETVAL
  </xsl:template>

  <xsl:template name="decypherDirectDeclarator">
    <xsl:param name="hslIdentifier" />
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

