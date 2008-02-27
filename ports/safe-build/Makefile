USRDIR=/usr
MANDIR=$(USRDIR)/man
ETCDIR=/etc
SBINDIR=$(USRDIR)/sbin
DISTRO_VERSION=`crux | awk '{print $$3}'`

SRCS=safe_build safe_build_conf

all: $(SRCS)

safe_build:
	sed -i safe-build.in -e "s|@VERSION@|$(VERSION)|g"
	sed -i safe-build.in -e "s|@CRUX_VERSION@|$(DISTRO_VERSION)|g"
	mv safe-build.in safe-build

safe_build_conf:
	sed -i safe-build.conf -e "s|@CRUX_VERSION@|$(DISTRO_VERSION)|g"

install:
	install -d $(DESTDIR)/$(USRDIR)/share/safe-build/packages
	install -D -m 0755 safe-build $(DESTDIR)/$(SBINDIR)/safe-build
	install -D -m 0644 safe-build.conf $(DESTDIR)/$(ETCDIR)/safe-build.conf

clean: 
	rm -f *~

