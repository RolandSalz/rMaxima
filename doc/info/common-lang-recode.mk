include $(top_srcdir)/doc/info/common-lang.mk

## Converts foo.utf8 to foo
origlangsdir := $(basename $(langsdir))

## The strategy for copying (and transcoding) all the *.texi files
##
##  (1) Make maxima.texi depend on all other .texi files that have to
##      be copied across.
##
##  (2) Make a rule to create foo.texi from
##      ../$(origlangsdir)/foo.texi, which copies the file across and
##      recodes it.
##
##  (3) We give a rule to make maxima.info here

source_texis := $(wildcard ../$(origlangsdir)/*.texi)
dest_texis := $(notdir $(source_texis))
child_dest_texis := $(subst maxima.texi ,,$(dest_texis))

maxima.texi: $(child_dest_texis) texinfo.tex

texinfo.tex: ../$(origlangsdir)/texinfo.tex
	cp $< $@

## Find the input charset by reading from ../$(origlangsdir)/maxima.texi
input_charset := \
  $(shell grep @documentencoding <../$(origlangsdir)/maxima.texi \
	  | grep -v '^@c' | cut -d ' ' -f 2)
output_charset := UTF-8

recode_trans := $(input_charset)..$(output_charset)
iconv_trans := -f $(input_charset) -t $(output_charset)
sed_replace := 's/^@documentencoding.*/@documentencoding $(output_charset)/'

if USE_RECODE
    clone_cmd = recode $(recode_trans) <"$(1)" | sed $(sed_replace) >"$(2)";
else
    clone_cmd = iconv $(iconv_trans) "$(1)" | sed $(sed_replace) >"$(2)";
endif


## We transcode the input to Texinfo as we copy it across and make
## sure to change @documentencoding. The double colon makes the rule
## terminal, which avoids an infinite pattern matching loop.
%.texi:: ../$(origlangsdir)/%.texi
	$(call clone_cmd,$<,$@)
%.hhp:: ../$(origlangsdir)/%.hhp
	$(call clone_cmd,$<,$@)

## This variable gets the target added to clean-local in common.mk
CLEAN_RECODE=clean-texi
clean-texi:
	rm -f *.texi
	rm -f texinfo.tex
