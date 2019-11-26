GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/slimp
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/slimp

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
--link-lib-subdir=$(INSTALL_LIBRARY_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/slimp.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/slimp.gpr

check: all
	echo No tests yet

clean:
	gprclean -q -P gnat/slimp.gpr
