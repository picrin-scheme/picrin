repl: r7rs readline
	echo contrib/20.repl/repl.scm echo >> $(CONTRIB_LIBS_LIST)
	echo contrib/20.repl/repl.c >> $(CONTRIB_SRCS_LIST)
	echo repl >> $(CONTRIB_INITS_LIST)
