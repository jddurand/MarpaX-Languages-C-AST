<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:hsl="urn:hsl">
  <xsl:output method="text" omit-xml-declaration="yes" />

  <!-- =================================== -->

  <xsl:template match="/">
/*
 * C binding pseudo code of <xsl:value-of select="hsl:opt('module')"/>, generated <xsl:value-of select="hsl:localtime()"/>
 */

#define PERL_NO_GET_CONTEXT 1
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

/* ==================================================== */
/*                    USER INPUT                        */
/* ==================================================== */

<xsl:value-of select="hsl:content()"/><!-- left-aligned voluntarirly -->

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

  </xsl:template>

  <!-- =================================== -->

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
          <xsl:value-of select="concat('__ANON__STRUCT__', $topStructOrUnionSpecifierCounter)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!-- Call the template engine -->
    <xsl:call-template name="structOrUnion">
      <xsl:with-param name="identifier" select="$identifier"/>
      <xsl:with-param name="mode" select="$mode"/>
    </xsl:call-template>
    <!-- [%~ PROCESS structOrUnion('identifier', '<xsl:value-of select="$identifier"/>') ~%] -->
  </xsl:template>

  <!-- =================================== -->

  <xsl:template name="structOrUnion">
    <xsl:param name="identifier" />
    <xsl:param name="mode" />
    <xsl:variable name="hslIdentifier" select="concat(hsl:prefix(), $identifier)"/>
/* ---------------------------------------------------- */
    <xsl:choose>
      <xsl:when test="$mode = 'decl'">
typedef struct <xsl:value-of select="$hslIdentifier"/> {
  struct <xsl:value-of select="$identifier"/> inner;
  hsl_outer outer;
} <xsl:value-of select="$hslIdentifier"/>;
      </xsl:when>
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
        <xsl:for-each select="./*[local-name()='structDeclarationList']/*">
          <xsl:call-template name="structDeclaration"/>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <!-- =================================== -->

  <xsl:template name="structDeclaration">
HERE structDeclaration
  </xsl:template>

</xsl:stylesheet>

