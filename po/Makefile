# This file is part of RQDA


# After you add, delete or change any message in the source files, type
#    make
# to update the R-RQDA.pot file and the .po files.
#
# After you update the translations in any of the .po files, type
#    make
# to update the .mo files in ../inst/po/.../LC_MESSAGES


# To add a translation for a new language:
#
#   1. Add the new .po and .mo files to POFILES and MOFILES lists below.
#
#   2. Create the R-lang.po file using as template one of the .po files from
#      this directory.
#
#   3. Replace the translations with the new language translations.
#
#   4. Type in a terminal emulator (in this directory):
#      make


.POSIX:

all:
	echo 'library(tools) ; update_pkg_po("../../RQDA", bugs = "https://github.com/Ronggui/RQDA/issues")' | R --quiet --vanilla

clean:
	-rm -f *.po~ R-RQDA.pot

