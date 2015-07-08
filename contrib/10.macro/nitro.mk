CONTRIB_LIBS += $(wildcard contrib/10.macro/*.scm)

CONTRIB_TESTS += test-macro

test-macro: bin/picrin
	bin/picrin contrib/10.macro/t/ir-macro.scm
