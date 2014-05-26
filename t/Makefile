
load-dll: hello.so
	@../build/bin/picrin load-dll.scm

.PHONY: load-test clean

hello.so: hello_from_c.c
	$(CC) $(CFLAGS) -o hello.so hello_from_c.c -ldl -shared -fPIC -I ../include -I../extlib -std=c99

clean:
	rm -f hello.so
