# Unix makefile for calc example
MOSMLHOME=/usr/local/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=${MOSMLHOME}/bin/mosmllex
MOSMLYACC=${MOSMLHOME}/bin/mosmlyac

MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo
 
all:  tiger
tiger: 	tigermain.uo tigerabs.uo tigernlin.uo tigertips.uo tigertab.uo tigersres.uo tigerpp.uo tigerescap.uo	  \
			tigergrm.uo tigerlex.uo tigermisc.uo listpp.uo tigerpila.uo tigerseman.uo tigertree.uo tigertemp.uo 		\
			tigertrans.uo tigerit.uo tigerframe.uo tigercanon.uo tigerassem.uo tigercodegen.uo tigermap.uo 					\
		  tigerset.uo tigerliveness.uo tigerreg.uo  
			$(MOSMLL) -toplevel -o tiger tigermain.uo

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f tiger

# these rules are only needed if UNITS is undefined or empty
.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep $(UNITS) >> Makefile

### DO NOT DELETE THIS LINE
tigerset.uo: tigerset.ui 
tigertree.uo: tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigerreg.uo: tigermap.ui tigerpila.ui tigerframe.ui tigerassem.ui \
    tigertemp.ui tigerliveness.uo tigerset.ui 
tigermisc.uo: tigermisc.ui 
tigerliveness.uo: tigermap.ui tigerassem.ui tigertemp.ui tigerset.ui 
listpp.uo: tigersres.uo tigertips.uo 
tigerpp.uo: tigerabs.uo 
tigersres.uo: tigertab.ui tigertips.uo tigerabs.uo tigertrans.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigergrm.ui: tigerabs.uo 
tigerassem.uo: tigerassem.ui tigertemp.ui 
tigerassem.ui: tigertree.uo tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigerit.uo: tigertree.uo tigertab.ui tigertemp.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    topsort.uo tigerabs.uo tigermisc.ui listpp.uo tigertrans.ui 
tigerseman.ui: tigerabs.uo 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigercanon.ui \
    tigerassem.ui tigertemp.ui tigerreg.uo tigertrans.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigercanon.ui tigerassem.ui 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigercanon.ui tigerlex.uo tigertrans.ui tigerpp.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigertemp.uo: tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigerabs.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.ui tigertemp.ui 
tigerframe.ui: tigertree.uo tigerassem.ui tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigertemp.ui 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui tigertrans.ui 
tigertab.uo: tigertab.ui 
tigermap.uo: tigermap.ui 
