<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:hsl="urn:hsl">
  <xsl:output method="text" omit-xml-declaration="yes" />

  <!-- =================================================================== -->
  <!--                            MAIN                                     -->
  <!-- =================================================================== -->

  <xsl:template match="/">
    <xsl:variable name="input"     select="hsl:opt('input')" />
    <xsl:variable name="module"    select="hsl:opt('module')" />
    <xsl:variable name="localtime" select="hsl:opt('localtime')" />
    <xsl:if test="not($module)">
      <xsl:message terminate="yes">
Input was: <xsl:value-of select="$input" />
Please specify a module name using --targetopt module=xxx
      </xsl:message>
    </xsl:if>
/*
 * Perl5 binding of module: <xsl:value-of select="$module"/>.
 * Generated: <xsl:value-of select="$localtime"/>.
 */
#define PERL_NO_GET_CONTEXT 1
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

/* ==================================================== */
/*                    USER INPUT                        */
/* ==================================================== */
<xsl:value-of select="hsl:content()"/>

/* ==================================================== */
/*                      MACROS                          */
/* ==================================================== */

/* Target type constants */

/* C.f. DROLSKY/Params-Validate-1.18/lib/Params/Validate/XS.xs */
#define HSL_TARGET_TYPE_SCALAR    0x001
#define HSL_TARGET_TYPE_ARRAYREF  0x002
#define HSL_TARGET_TYPE_HASHREF   0x004
#define HSL_TARGET_TYPE_CODEREF   0x008
#define HSL_TARGET_TYPE_GLOB      0x010
#define HSL_TARGET_TYPE_GLOBREF   0x020
#define HSL_TARGET_TYPE_SCALARREF 0x040
#define HSL_TARGET_TYPE_UNKNOWN   0x080
#define HSL_TARGET_TYPE_UNDEF     0x100
#define HSL_TARGET_TYPE_OBJECT    0x200
#define HSL_TARGET_TYPE_HANDLE    (HSL_TARGET_TYPE_GLOB   | HSL_TARGET_TYPE_GLOBREF)
#define HSL_TARGET_TYPE_BOOLEAN   (HSL_TARGET_TYPE_SCALAR | HSL_TARGET_TYPE_UNDEF)

/* Target type management */
#define HSL_TARGET_NEW_ARRAY(dst)     do { dst = newAV(); } while (0)
#define HSL_TARGET_DEL_ARRAY(src)     do { av_undef((AV *)src); src = NULL; } while (0)
#define HSL_TARGET_IS_ARRAY(dst, src) do { dst = ((get_type(aTHX_ (SV *)pattern) &amp; HSL_TARGET_TYPE_ARRAYREF) == HSL_TARGET_TYPE_ARRAYREF) ? 1 : 0 } while (0)

/* ==================================================== */
/*                    DECLARATIONS                      */
/* ==================================================== */
typedef struct hsl_outer {
  short allocated; /* Heuristic tentative to protect against userland error */
} hsl_outer;
static IV get_type(pTHX_ SV* sv);

<xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
  <xsl:call-template name="topStructOrUnionSpecifier">
    <xsl:with-param name="module" select="$module"/>
    <xsl:with-param name="topStructOrUnionSpecifierCounter" select="position()"/>
    <xsl:with-param name="mode" select="'decl'"/>
  </xsl:call-template>
</xsl:for-each>

/* ==================================================== */
/*                    DEFINITIONS                       */
/* ==================================================== */
<xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
  <xsl:call-template name="topStructOrUnionSpecifier">
    <xsl:with-param name="module" select="$module"/>
    <xsl:with-param name="topStructOrUnionSpecifierCounter" select="position()"/>
    <xsl:with-param name="mode" select="'def'"/>
  </xsl:call-template>
</xsl:for-each>

static
IV
get_type(pTHX_ SV* sv) {
  IV type = 0;

  if (SvTYPE(sv) == SVt_PVGV) {
    return HSL_TARGET_TYPE_GLOB;
  }
  if (!SvOK(sv)) {
    return HSL_TARGET_TYPE_UNDEF;
  }
  if (!SvROK(sv)) {
    return HSL_TARGET_TYPE_SCALAR;
  }

  switch (SvTYPE(SvRV(sv))) {
  case SVt_NULL:
  case SVt_IV:
  case SVt_NV:
  case SVt_PV:
#if PERL_VERSION &lt;= 10
  case SVt_RV:
#endif
  case SVt_PVMG:
  case SVt_PVIV:
  case SVt_PVNV:
#if PERL_VERSION &lt;= 8
  case SVt_PVBM:
#elif PERL_VERSION >= 11
  case SVt_REGEXP:
#endif
    type = HSL_TARGET_TYPE_SCALARREF;
    break;
  case SVt_PVAV:
    type = HSL_TARGET_TYPE_ARRAYREF;
    break;
  case SVt_PVHV:
    type = HSL_TARGET_TYPE_HASHREF;
    break;
  case SVt_PVCV:
    type = HSL_TARGET_TYPE_CODEREF;
    break;
  case SVt_PVGV:
    type = HSL_TARGET_TYPE_GLOBREF;
    break;
    /* Perl 5.10 has a bunch of new types that I don't think will ever
       actually show up here (I hope), but not handling them makes the
       C compiler cranky. */
  default:
    type = HSL_TARGET_TYPE_UNKNOWN;
    break;
  }

  if (type) {
    if (sv_isobject(sv)) return (type | HSL_TARGET_TYPE_OBJECT);
    return type;
  }

  /* Getting here should not be possible */
  return HSL_TARGET_TYPE_UNKNOWN;
}

/* ==================================================== */
/*                     INTERFACE                        */
/* ==================================================== */
<xsl:for-each select=".//structOrUnionSpecifier[not (ancestor::*[self::structDeclarationList])]">
  <xsl:call-template name="topStructOrUnionSpecifier">
    <xsl:with-param name="module" select="$module"/>
    <xsl:with-param name="topStructOrUnionSpecifierCounter" select="position()"/>
    <xsl:with-param name="mode" select="'ifce'"/>
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
      <xsl:with-param name="module" select="$module"/>
      <xsl:with-param name="identifier" select="$identifier"/>
      <xsl:with-param name="mode" select="$mode"/>
    </xsl:call-template>
  </xsl:template>

  <!-- =================================================================== -->
  <!--                   structOrUnionSpecifier                            -->
  <!-- =================================================================== -->

  <xsl:template name="structOrUnionSpecifier">
    <xsl:param name="module" />
    <xsl:param name="identifier" />
    <xsl:param name="mode" />
    <xsl:variable name="hslIdentifier" select="concat(hsl:prefix(), $identifier)"/>
/* ---------------------------------------------------- */
    <xsl:choose>
      <!--  Structure Or Union: Declarations   -->
      <xsl:when test="$mode = 'decl'">
typedef struct <xsl:value-of select="$hslIdentifier"/> {
  <xsl:value-of select="./@text"/> inner;
  hsl_outer outer;
} <xsl:value-of select="$hslIdentifier"/>;
void *<xsl:value-of select="$hslIdentifier"/>_new(pTHX_ char *classString);
void <xsl:value-of select="$hslIdentifier"/>_DESTROY(pTHX_ void *self);
      </xsl:when>
      <!--  Structure Or Union: Definitions    -->
      <xsl:when test="$mode = 'def'">
int <xsl:value-of select="$hslIdentifier"/>_new(pTHX_ char *classString, <xsl:value-of select="$hslIdentifier"/> **selfPtrPtr) {
  Newx(*selfPtrPtr, 1, <xsl:value-of select="$hslIdentifier"/>);
  return 1;
}
int <xsl:value-of select="$hslIdentifier"/>_DESTROY(pTHX_ void **selfPtrPtr) {
  Safefree(*selfPtrPtr);
  return 1;
}
      </xsl:when>
      <!--  Structure Or Union: Interface    -->
      <xsl:when test="$mode = 'ifce'">
MODULE = <xsl:value-of select="$module"/>	PACKAGE = <xsl:value-of select="concat($module, '::', $identifier)"/>

PROTOTYPES: ENABLE
        <!-- Generate creator, destructor -->
void *
<xsl:value-of select="$hslIdentifier"/>_new(pTHX_ char *classString)
PREINIT:
  <xsl:value-of select="$hslIdentifier"/> *selfPtr;
CODE:
  if (! <xsl:value-of select="$hslIdentifier"/>_new(aTHX_ classString, &amp;selfPtr)) {
    RETVAL = PL_undef;
  } else {
    RETVAL = selfPtr;
  }
OUTPUT: 
  RETVAL 

void 
DESTROY(pTHX_ void *self)
PREINIT:
CODE: 
  Safefree(self); 
        <!-- Generate accessors -->
        <!-- we are only interested by declarations that declare something -->
        <xsl:for-each select="./structDeclarationList/*/structDeclaratorList/*/declarator">
          <!-- In addition we are absolutely NOT interested by the type of what
               we return: we always return the address of the element. Full point. -->
          <xsl:call-template name="declarator">
            <xsl:with-param name="module" select="$module"/>
            <xsl:with-param name="mode" select="$mode" />
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
    <xsl:param name="module" />
    <xsl:param name="mode" />
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
      <xsl:with-param name="mode" select="$mode" />
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
