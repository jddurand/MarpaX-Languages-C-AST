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
    <xsl:variable name="module"    select="hsl:module()" />
    <xsl:variable name="localtime" select="hsl:localtime()" />
    <xsl:if test="not($module)">
      <xsl:message terminate="yes">
Input was: <xsl:value-of select="$content" />
Please specify a module name using --targetopt module=xxx
      </xsl:message>
    </xsl:if>
/*
 * Perl5 bindings of module <xsl:value-of select="$module"/>.
 * Generated <xsl:value-of select="$localtime"/>.
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
/*                  INTERNAL TYPEDEFS                   */
/* ==================================================== */
<xsl:for-each select="./newTypedefs/*">
typedef<xsl:text> </xsl:text><xsl:value-of select="./@type"/><xsl:text>
</xsl:text><xsl:value-of select="./@def" /><xsl:text>
</xsl:text><xsl:value-of select="./@name" />;
</xsl:for-each>

/* ==================================================== */
/*                      MACROS                          */
/* ==================================================== */
/*
 * C and target type definitions
 * -----------------------------
 */
#define HSL_C_TYPE_BOOLEAN          short

/* C.f. DROLSKY/Params-Validate-1.18/lib/Params/Validate/XS.xs */
#define HSL_TARGET_TYPE_SCALAR      0x001
#define HSL_TARGET_TYPE_ARRAYREF    0x002
#define HSL_TARGET_TYPE_HASHREF     0x004
#define HSL_TARGET_TYPE_CODEREF     0x008
#define HSL_TARGET_TYPE_GLOB        0x010
#define HSL_TARGET_TYPE_GLOBREF     0x020
#define HSL_TARGET_TYPE_SCALARREF   0x040
#define HSL_TARGET_TYPE_UNKNOWN     0x080
#define HSL_TARGET_TYPE_UNDEF       0x100
#define HSL_TARGET_TYPE_OBJECT      0x200
#define HSL_TARGET_TYPE_HANDLE      (HSL_TARGET_TYPE_GLOB   | HSL_TARGET_TYPE_GLOBREF)
#define HSL_TARGET_TYPE_BOOLEAN     (HSL_TARGET_TYPE_SCALAR | HSL_TARGET_TYPE_UNDEF)

/*
 * C and target type boolean tests
 * -------------------------------
 */
#define HSL_C_IS_TRUE(x)          ((x) != 0)
#define HSL_C_IS_FALSE(x)         ((x) == 0)

#define HSL_TARGET_IS_TYPE(x, t) ((get_type(aTHX_ (SV *)(x)) &amp; (t)) == (t))
#define HSL_TARGET_IS_BOOLEAN(x)  HSL_TARGET_IS_TYPE((x), HSL_TARGET_TYPE_BOOLEAN)
#define HSL_TARGET_IS_ARRAY(x)    HSL_TARGET_IS_TYPE((x), HSL_TARGET_TYPE_ARRAYREF)
#define HSL_TARGET_IS_STRUCT(x)   HSL_TARGET_IS_TYPE((x), HSL_TARGET_TYPE_HASHREF)
#define HSL_TARGET_IS_FUNC(x)     HSL_TARGET_IS_TYPE((x), HSL_TARGET_TYPE_CODEREF)

/*
 * C and target type macros as functions
 * -------------------------------------
 */
#define HSL_TARGET_NEW_ARRAY(dst)     do { (dst) = newAV(); } while (0)
#define HSL_TARGET_DEL_ARRAY(src)     do { av_undef((AV *)(src)); (src) = NULL; } while (0)

/* ==================================================== */
/*                    DECLARATIONS                      */
/* ==================================================== */
typedef struct hsl_outer {
  short allocated; /* Heuristic tentative to protect against userland error */
} hsl_outer;
static IV get_type(pTHX_ SV* sv);
<xsl:for-each select="./newTypedefs/*">
static HSL_C_TYPE_BOOLEAN <xsl:value-of select="./@name" />_new(aTHX_ <xsl:value-of select="./@name" /> **thispp);
static HSL_C_TYPE_BOOLEAN <xsl:value-of select="./@name" />_free(aTHX_ <xsl:value-of select="./@name" /> *thisp);
</xsl:for-each>
<xsl:for-each select="./cdecl/identifiers/*">
  <xsl:variable name="identifier" select="." />
  <xsl:for-each select="$identifier/*">
    <xsl:variable name="member" select="." />
static HSL_C_TYPE_BOOLEAN <xsl:value-of select="$identifier/@name" />_<xsl:value-of select="$member/@name" />_get(aTHX_ <xsl:value-of select="$identifier/@name" /> *thisp, void **rcpp);
static HSL_C_TYPE_BOOLEAN <xsl:value-of select="$identifier/@name" />_<xsl:value-of select="$member/@name" />_set(aTHX_ <xsl:value-of select="$identifier/@name" /> *thisp, void *rcp);
  </xsl:for-each>
</xsl:for-each>

/* ==================================================== */
/*                    DEFINITIONS                       */
/* ==================================================== */
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

  </xsl:template>
</xsl:stylesheet>

