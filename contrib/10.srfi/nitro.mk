srfi: srfi1 srfi8 srfi17 srfi26 srfi43 srfi60 srfi95 srfi111
srfi1: r7rs
	echo contrib/10.srfi/srfi/1.scm >> $(CONTRIB_LIBS_LIST)
srfi8: r7rs
	echo contrib/10.srfi/srfi/8.scm >> $(CONTRIB_LIBS_LIST)
srfi17: r7rs srfi1 srfi8
	echo contrib/10.srfi/srfi/17.scm >> $(CONTRIB_LIBS_LIST)
srfi26: r7rs srfi1
	echo contrib/10.srfi/srfi/26.scm >> $(CONTRIB_LIBS_LIST)
srfi43: r7rs srfi8
	echo contrib/10.srfi/srfi/43.scm >> $(CONTRIB_LIBS_LIST)
srfi60: r7rs srfi1
	echo contrib/10.srfi/srfi/60.scm >> $(CONTRIB_LIBS_LIST)
srfi95: r7rs srfi1
	echo contrib/10.srfi/srfi/95.scm >> $(CONTRIB_LIBS_LIST)
srfi111: r7rs
	echo contrib/10.srfi/srfi/111.scm >> $(CONTRIB_LIBS_LIST)
