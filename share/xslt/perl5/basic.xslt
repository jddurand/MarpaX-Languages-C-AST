<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  <xsl:output omit-xml-declaration="yes" indent="yes" encoding="utf-8"/>
  <xsl:template match="C">
    <xsl:text>
#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
</xsl:text>
    <xsl:apply-templates select="decls"/>
  </xsl:template>

  <xsl:template match="decls">
    <xsl:text>Name: </xsl:text><xsl:value-of select="@nm"/><xsl:text>&#xa;</xsl:text>
  </xsl:template>

</xsl:stylesheet>
