
COBC=cobc
COBFLAGS=-g

COBCPY=/opt/cobol/gixsql/share/gixsql/copy
GIXPPFLAGS= --copybooks -I $(COBCPY)
GIXPP = ~/Projects/superbol-studio-oss/superbol-free-linux-x64 sql preproc

all: CREATEDB.exe SUMDB.exe

CREATEDB.exe: CREATEDB.cob
	$(GIXPP) $(GIXPPFLAGS) ./CREATEDB.cob
	$(COBC) -lgixsql -x -o CREATEDB.exe CREATEDB.pp.cob

SUMDB.exe: SUMDB.cob
	$(GIXPP) $(GIXPPFLAGS) ./SUMDB.cob
	$(COBC) -lgixsql -x -o SUMDB.exe SUMDB.pp.cob

clean:
	rm -f *~ *.log *.pp.cob *.o *.so *.exe

