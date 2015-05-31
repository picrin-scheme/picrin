include contrib/*/nitro.mk

contrib: | clean-contrib contrib-real
contrib-real: callcc r7rs optional partcont pretty-print random readline regexp srfi for repl main class protocol
clean-contrib:
	rm -f $(CONTRIB_LIBS_LIST) $(CONTRIB_SRCS_LIST) $(CONTRIB_INITS_LIST) $(CONTRIB_TESTS_LIST)
