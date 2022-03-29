
LIB=GEOTAB
TGTRLS=*CURRENT

all: geotab.srvpgm geotab.bnddir

geotab.srvpgm: geotab.sqlmod geotab.header
geotab2.srvpgm: geotab2.sqlmod
geotab.sqlmod: geotab.bnddir
geotab.bnddir: geotab.entry jsonxml.entry

%.srvpgm:
	system "CRTSRVPGM SRVPGM($(LIB)/$*) MODULE($(patsubst %,$(LIB)/%,$(basename $^))) EXPORT(*ALL) BNDSRVPGM(($(LIB)/JSONXML)) TGTRLS($(TGTRLS))"

%.sqlmod: src/%.sqlrpgle
	system -s "CHGATR OBJ('$<') ATR(*CCSID) VALUE(1252)"
	system -s "CRTSQLRPGI OBJ($(LIB)/$*) SRCSTMF('$<') COMMIT(*NONE) OBJTYPE(*MODULE) OPTION(*EVENTF *XREF) DBGVIEW(*SOURCE) TGTRLS($(TGTRLS))"

%.bnddir:
	-system -s "CRTBNDDIR BNDDIR($(LIB)/$*)"
	-system -s "ADDBNDDIRE BNDDIR($(LIB)/$*) OBJ($(patsubst %.entry,(*LIBL/% *SRVPGM *IMMED),$^))"

%.entry:
	@echo "Binding entry: $*"

%.header: headers/%.rpgle_h
	-system -s "CRTSRCPF FILE($(LIB)/QRPGLEREF) RCDLEN(112)"
	system -s "CPYFRMSTMF FROMSTMF('$<') TOMBR('/QSYS.LIB/$(LIB).LIB/QRPGLEREF.FILE/$*.MBR') MBROPT(*REPLACE)"

%.package:
	-system -s "DLTOBJ OBJ($(LIB)/PACKAGE) OBJTYPE(*FILE)"
	system -s "CRTSAVF FILE($(LIB)/PACKAGE)"
	system -s "SAV DEV('/QSYS.LIB/$(LIB).LIB/PACKAGE.FILE') OBJ(('/QSYS.LIB/$(LIB).LIB/$*.SRVPGM')) TGTRLS($(TGTRLS))"

	-mkdir pkgs
	system -s "CPYTOSTMF FROMMBR('/QSYS.lib/$(LIB).lib/PACKAGE.FILE') TOSTMF('./pkgs/$*.pkg') STMFOPT(*REPLACE) STMFCCSID(1252) CVTDTA(*NONE)"
	system -s "DLTOBJ OBJ($(LIB)/PACKAGE) OBJTYPE(*FILE)"

%.restore:
	system -s "CPYFRMSTMF FROMSTMF('./pkgs/$*.pkg') TOMBR('/QSYS.LIB/$(LIB).LIB/PACKAGE.FILE') MBROPT(*REPLACE) CVTDTA(*NONE)"
	system -s "RST DEV('/QSYS.LIB/$(LIB).LIB/PACKAGE.FILE') OBJ(('/QSYS.LIB/$(LIB).LIB/$*.SRVPGM'))"
	system -s "DLTOBJ OBJ($(LIB)/PACKAGE) OBJTYPE(*FILE)"