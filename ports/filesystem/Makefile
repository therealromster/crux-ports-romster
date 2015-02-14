all:
	nasm -felf64 crux.asm -o crux.o
	ld crux.o -o crux
	strip crux

clean:
	rm crux

install:
	install -m 0755 -D crux $(DESTDIR)/usr/bin/crux

