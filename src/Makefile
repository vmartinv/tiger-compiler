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
 	tigergraph.uo tigerflow.uo tigerliveness.uo tigersimpleregalloc.uo tigerregalloc.uo tigercolor.uo

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
tigerliveness.uo: tigerliveness.ui tigergraph.ui tigertemp.ui tigerutils.ui \
    tigerflow.ui 
tigermap.uo: tigermap.ui 
tigerregalloc.uo: tigerregalloc.ui tigerframe.ui tigertemp.ui tigerutils.ui 
tigerutils.uo: tigerutils.ui 
tigerpp.uo: tigertips.uo tigerframe.ui tigerabs.uo tigerutils.ui 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui tigerutils.ui 
tigerinclude.uo: tigerinclude.ui tigergrm.ui tigernlin.uo tigerabs.uo \
    tigerlex.uo tigerutils.ui 
tigerflow.uo: tigerflow.ui tigergraph.ui tigerassem.ui tigertemp.ui \
    tigerutils.ui 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigerflow.ui: tigergraph.ui tigerassem.ui tigertemp.ui 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.ui tigertemp.ui 
tigercolor.ui: tigerframe.ui tigerassem.ui tigertemp.ui 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigerframe.ui tigercanon.ui tigerassem.ui tigerinclude.ui tigerlex.uo \
    tigerliveness.ui tigerutils.ui tigertrans.ui tigerflow.ui tigerpp.uo 
tigerframe.ui: tigertree.uo tigerassem.ui tigertemp.ui 
tigertab.uo: tigertab.ui 
tigerassem.uo: tigerassem.ui tigertemp.ui tigerutils.ui 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerassem.ui \
    tigertemp.ui tigerutils.ui 
tigertree.uo: tigertemp.ui 
tigersimpleregalloc.uo: tigersimpleregalloc.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.ui 
tigerseman.ui: tigerabs.uo 
tigerinclude.ui: tigerabs.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo tigerutils.ui 
tigerit.uo: tigertree.uo tigertab.ui tigerutils.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigerutils.ui tigertrans.ui \
    tigerpp.uo 
tigerregalloc.ui: tigerframe.ui tigerassem.ui tigertemp.ui 
tigerassem.ui: tigertemp.ui 
tigergrm.ui: tigerabs.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigerset.uo: tigerset.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo tigerutils.ui 
tigerliveness.ui: tigergraph.ui tigertemp.ui tigerflow.ui 
tigergraph.uo: tigergraph.ui tigerutils.ui 
tigerescap.ui: tigerabs.uo 
tigercolor.uo: tigercolor.ui tigermap.ui tigerpila.ui tigerframe.ui \
    tigerassem.ui tigertemp.ui tigerliveness.ui tigerset.ui 
tigertemp.uo: tigertemp.ui tigerutils.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigerit.uo tigerassem.ui tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigerassem.ui tigertemp.ui tigerabs.uo tigerutils.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigerpila.uo: tigerpila.ui 
