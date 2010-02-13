default:
	@ echo
	@ echo "It is NOT recommended that you use this Makefile to install"
	@ echo "the NVIDIA driver.  Instead, please use the nvidia-installer"
	@ echo "utility.  However, should nvidia-installer not operate for you,"
	@ echo "you may use this Makefile as a last resort.  To install using"
	@ echo "this Makefile, please run 'make install'."
	@ echo

kernel_module_install:
	cd usr/src/nv; make install


ROOT=
DSOMAJOR=1
DSOMINOR=0
DSOPATCH=9755
DSOREV=$(DSOMAJOR).$(DSOMINOR).$(DSOPATCH)

DOCDIR=${ROOT}/usr/share/doc/NVIDIA_GLX-$(DSOMAJOR).$(DSOMINOR)

INSTALL_TLS := $(shell usr/bin/tls_test usr/bin/tls_test_dso.so 2> /dev/null; if [ "$$?" = "0" ]; then echo "1"; else echo "0"; fi)

install: kernel_module_install
	@echo "Removing all old and conflicting files!"
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/libGL.*
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/libGLcore.*
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/libGLwrapper.*
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/libnvidia-tls.*
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/libnvidia-cfg.*
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/tls/libGL.*
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/tls/libGLcore.*
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/tls/libGLwrapper.*
	@echo -n "      "
	rm -f ${ROOT}/usr/lib/tls/libnvidia-tls.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/libGL.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/libGLcore.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/libXvMCNVIDIA.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/libGLwrapper.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/tls/libGL.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/tls/libGLcore.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/tls/libXvMCNVIDIA.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/tls/libGLwrapper.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/modules/extensions/libGLcore.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/modules/extensions/libglx.*
	@echo -n "      "
	rm -f ${ROOT}/usr/X11R6/lib/modules/extensions/libGLwrapper.*
	@echo "Installing new drivers"
	@echo -n "      "
	install usr/lib/libGL.so.${DSOREV} ${ROOT}/usr/lib
	@echo -n "      "
	install usr/lib/libGLcore.so.${DSOREV} ${ROOT}/usr/lib
	@echo -n "      "
	install usr/X11R6/lib/modules/drivers/nvidia_drv.o ${ROOT}/usr/X11R6/lib/modules/drivers
	@echo -n "      "
	install usr/X11R6/lib/modules/extensions/libglx.so.${DSOREV} ${ROOT}/usr/X11R6/lib/modules/extensions
	@echo -n "      "
	install usr/lib/libnvidia-tls.so.${DSOREV} ${ROOT}/usr/lib
	@if [ "$(INSTALL_TLS)" = "1" ]; then \
		mkdir -p ${ROOT}/usr/lib/tls; \
		echo "      install usr/lib/tls/libnvidia-tls.so.${DSOREV} ${ROOT}/usr/lib/tls"; \
		install usr/lib/tls/libnvidia-tls.so.${DSOREV} ${ROOT}/usr/lib/tls; \
	fi
	@echo -n "      "
	install usr/lib/libnvidia-cfg.so.${DSOREV} ${ROOT}/usr/lib
	@if [ -f usr/X11R6/lib/libXvMCNVIDIA.a ]; then \
		echo "      install usr/X11R6/lib/libXvMCNVIDIA.a ${ROOT}/usr/X11R6/lib/libXvMCNVIDIA.a"; \
		install usr/X11R6/lib/libXvMCNVIDIA.a ${ROOT}/usr/X11R6/lib/libXvMCNVIDIA.a; \
		echo "      install usr/X11R6/lib/libXvMCNVIDIA.so.${DSOMAJOR}.${DSOMINOR}.${DSOPATCH} ${ROOT}/usr/X11R6/lib/libXvMCNVIDIA.so.${DSOMAJOR}.${DSOMINOR}.${DSOPATCH}"; \
		install usr/X11R6/lib/libXvMCNVIDIA.so.${DSOMAJOR}.${DSOMINOR}.${DSOPATCH} ${ROOT}/usr/X11R6/lib/libXvMCNVIDIA.so.${DSOMAJOR}.${DSOMINOR}.${DSOPATCH}; \
	fi
	@echo -n "      "
	cd ${ROOT}/usr/lib/; ln -fs libGL.so.${DSOREV} libGL.so
	@echo -n "      "
	cd ${ROOT}/usr/X11R6/lib/modules/extensions; ln -fs libglx.so.${DSOREV} libglx.so
	@echo "Installing documentation in ${DOCDIR}"
	@echo -n "      "
	mkdir -p ${DOCDIR}/include
	@echo -n "      "
	cp -r usr/include/* ${DOCDIR}/include
	@echo -n "      "
	cp -r usr/share/doc/* ${DOCDIR}
	@echo -n "      "
	/sbin/ldconfig
