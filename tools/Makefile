all: mkboot built-in.scm
	cat built-in.scm | ./mkboot > ../extlib/benz/boot.c

mkboot: mkboot.c
	$(CC) -I../extlib/benz/include mkboot.c ../extlib/benz/*.c -o mkboot

clean:
	rm -f mkboot
