include contrib/*/nitro.mk

CONTRIBS := callcc optional partcont pretty-print random readline regexp srfi for repl main class protocol

contrib: | contrib-clean contrib-real
contrib-real: $(CONTRIBS)
contrib-clean:
	rm -f $(CONTRIB_LIBS_LIST) $(CONTRIB_SRCS_LIST) $(CONTRIB_INITS_LIST) $(CONTRIB_TESTS_LIST)

.PHONY: contrib contrib-real contrib-clean
