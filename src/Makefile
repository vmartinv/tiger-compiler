# Unix makefile for tigermain example

ifeq ($(wildcard /usr/local/bin/mosmlc),) 
    BASEDIR=/usr
else 
    BASEDIR=/usr/local
endif 

#BASEDIR=$(dir $(dir $(which mosmlc)))
HOME=${BASEDIR}/bin
MOSMLHOME=${HOME}
MOSMLTOOLS=camlrunm ${BASEDIR}/share/mosml/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=${MOSMLHOME}/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
 	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo tigertemp.uo tigertopsort.uo tigerutils.uo tigertree.uo \
 	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo tigerinclude.uo tigercanon.uo \
 	tigergraph.uo tigermakegraph.uo  tigersimpleregalloc.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o
	$(REMOVE) tiger

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml tigertopsort.sml tigerutils.sml tigerinclude.sml \
	tigergraph.sml tigermakegraph.sml tigersimpleregalloc.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigersimpleregalloc.uo: tigersimpleregalloc.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerinclude.uo: tigerinclude.ui tigergrm.ui tigernlin.uo tigerabs.uo \
    tigerlex.uo tigerutils.ui 
tigertree.uo: tigertemp.ui 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigercanon.ui \
    tigerinclude.ui tigerlex.uo tigerutils.ui tigertrans.ui tigerpp.uo 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigerutils.ui tigertrans.ui \
    tigerpp.uo 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigerutils.uo: tigerutils.ui 
tigerframe.uo: tigerframe.ui tigertree.uo tigertemp.ui 
tigerseman.ui: tigerabs.uo 
tigertab.uo: tigertab.ui 
tigerinclude.ui: tigerabs.uo 
tigergrm.ui: tigerabs.uo 
tigerpila.uo: tigerpila.ui 
tigertemp.uo: tigertemp.ui 
tigerpp.uo: tigertips.uo tigerframe.ui tigerabs.uo tigerutils.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo tigerutils.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo tigerutils.ui 
tigerframe.ui: tigertree.uo tigertemp.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigergraph.uo: tigergraph.ui tigerutils.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigerit.uo tigertemp.ui 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigermakegraph.uo: tigermakegraph.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigerescap.ui: tigerabs.uo 
