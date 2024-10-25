
COBC=cobc
COBFLAGS=-g

COBCPY=/opt/cobol/gixsql/share/gixsql/copy
GIXPPFLAGS=-e -S -I $(COBCPY)
GIXPP=gixpp

all: createdb.exe sumdb.exe

createdb.exe: createdb.cob
	$(GIXPP) $(GIXPPFLAGS) -i ./createdb.cob -o createdb.pp.cob
	$(COBC) -lgixsql -x -o createdb.exe createdb.pp.cob

sumdb.exe: sumdb.cob
	$(GIXPP) $(GIXPPFLAGS) -i ./sumdb.cob -o sumdb.pp.cob
	$(COBC) -lgixsql -x -o sumdb.exe sumdb.pp.cob

clean:
	rm -f *~ *.log *.pp.cob *.o *.so *.exe

