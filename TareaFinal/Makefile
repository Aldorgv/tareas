
INCDIR=include
SRCDIR=src

IMPLFILES=letrec.tab.c letrec.lex.c ast.c parse.c driver.c

CFLAGS =-std=c17 \
	-I$(INCDIR)/ \
	-g3 -O0 \
	-DYYDEBUG=1


Release: letrec.c $(addprefix $(SRCDIR)/,$(patsubst %.c,%.o,$(IMPLFILES)))
	gcc $(CFLAGS) -o letrec\bin\Release\letrec.exe $^



run: Release
	./letrec

%.o: %.c
	gcc $(CFLAGS) -o $@ -c $<

$(SRCDIR)/letrec.lex.c: $(SRCDIR)/letrec.l
	flex -o$@ $<

$(SRCDIR)/letrec.tab.c: $(SRCDIR)/letrec.y
	bison -o $@ -d $<

cleanRelease:
	rm -rf src/*.o src/*.lex.c src/*.tab.*

ejemplo00: compile ejemplo00.txt
	./letrec ejemplo00.txt
