# Unix makefile for tigermain example

#BASEDIR=/usr/local
BASEDIR=$(dir $(dir $(which mosmlc)))
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
 	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo tigerimport.uo

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
	tigernlin.sml tigerpp.sml tigertopsort.sml tigerutils.sml tigerimport.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigerpp.ui: tigertips.uo
tigerpp.uo: tigerabs.uo tigertips.uo tigerutils.ui
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigerframe.ui: tigertree.uo tigertemp.ui 
tigerframe.uo: tigerframe.ui tigertree.uo tigertemp.ui 
tigergrm.ui: tigerabs.uo tigernlin.uo tigerabs.uo 
tigerseman.ui: tigersres.uo tigertab.ui tigertips.uo tigerabs.uo tigertopsort.uo tigerutils.ui
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerit.uo: tigertree.uo tigertab.ui 
tigertree.uo: tigertemp.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigerabs.uo tigertrans.ui tigerpp.ui
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigerpila.uo: tigerpila.ui 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigertab.uo: tigertab.ui 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigerlex.uo \
    tigerpp.uo tigerimport.ui
tigertemp.uo: tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo
tigerutils.uo: tigerutils.ui
tigerimport.ui: tigerabs.ui
tigerimport.uo: tigerimport.ui tigerabs.ui
