DIRS	= SWAT2005 SWAT2009
ECHO	= echo

all : swat05 swat09

swat05 : force_look
	$(ECHO) looking into swat2005: $(MAKE) $(MFLAGS)
	cd SWAT2005; $(MAKE) $(MFLAGS)

swat09 : force_look
	$(ECHO) looking into swat2005: $(MAKE) $(MFLAGS)
	cd SWAT2009; $(MAKE) $(MFLAGS)

clean :
	$(ECHO) cleaning up in .
	-rm -f $(EXE) $(OBJS) $(OBJLIBS)
	-for d in $(DIRS); do (cd $$d; $(MAKE) clean ); done

force_look :
	true
