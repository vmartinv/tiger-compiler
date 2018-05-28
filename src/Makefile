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
 	tigerassem.uo \ tigercodegen.uo \
 	tigergraph.uo tigerflow.uo tigerliveness.uo tigersimpleregalloc.uo

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
	tigerassem.sml \
	tigergraph.sml tigerflow.sml tigerliveness.sml tigersimpleregalloc.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigerframe.uo: tigerframe.ui tigertree.uo tigertemp.ui 
tigergrm.ui: tigerabs.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigertree.uo: tigertemp.ui 
tigerseman.ui: tigerabs.uo 
tigerassem.ui: tigertemp.ui 
tigerframe.ui: tigertree.uo tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo tigerutils.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo tigerutils.ui 
tigerassem.uo: tigerassem.ui tigertemp.ui 
tigertemp.uo: tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigerinclude.ui: tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigerliveness.uo: tigerliveness.ui tigergraph.ui tigertemp.ui tigerutils.ui \
    tigerflow.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigerutils.uo: tigerutils.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.ui 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigerflow.uo: tigerflow.ui tigergraph.ui tigerassem.ui tigertemp.ui \
    tigerutils.ui 
tigerliveness.ui: tigergraph.ui tigertemp.ui tigerflow.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui 
tigercodegen.uo: tigercodegen.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigertab.uo: tigertab.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigerutils.ui tigertrans.ui \
    tigerpp.uo 
tigerflow.ui: tigergraph.ui tigerassem.ui tigertemp.ui 
tigerinclude.uo: tigerinclude.ui tigergrm.ui tigernlin.uo tigerabs.uo \
    tigerlex.uo tigerutils.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigerit.uo tigertemp.ui 
tigergraph.uo: tigergraph.ui tigerutils.ui 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigercanon.ui tigerinclude.ui tigerlex.uo tigerutils.ui tigertrans.ui \
    tigerpp.uo 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigerpp.uo: tigertips.uo tigerframe.ui tigerabs.uo tigerutils.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
