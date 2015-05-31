callcc:
	echo callcc >> $(CONTRIB_INITS_LIST)
	echo $(wildcard contrib/03.callcc/*.c) >> $(CONTRIB_SRCS_LIST)
