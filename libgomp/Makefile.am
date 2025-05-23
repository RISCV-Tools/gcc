## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = info-in-builddir
ACLOCAL_AMFLAGS = -I .. -I ../config
SUBDIRS = testsuite

## May be used by toolexeclibdir.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

config_path = @config_path@
search_path = $(addprefix $(top_srcdir)/config/, $(config_path)) $(top_srcdir) \
	      $(top_srcdir)/../include

fincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)$(MULTISUBDIR)/finclude
libsubincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

vpath % $(strip $(search_path))

AM_CPPFLAGS = $(addprefix -I, $(search_path))
AM_CFLAGS = $(XCFLAGS)
AM_LDFLAGS = $(XLDFLAGS) $(SECTION_LDFLAGS) $(OPT_LDFLAGS)

toolexeclib_LTLIBRARIES = libgomp.la
nodist_toolexeclib_HEADERS = libgomp.spec

if LIBGOMP_BUILD_VERSIONED_SHLIB
# -Wc is only a libtool option.
comma = ,
PREPROCESS = $(subst -Wc$(comma), , $(COMPILE)) -E

libgomp.ver: $(top_srcdir)/libgomp.map
	$(EGREP) -v '#(#| |$$)' $< | \
	  $(PREPROCESS) -P -include config.h - > $@ || (rm -f $@ ; exit 1)

if LIBGOMP_BUILD_VERSIONED_SHLIB_GNU
libgomp_version_script = -Wl,--version-script,libgomp.ver
libgomp_version_dep = libgomp.ver
endif
if LIBGOMP_BUILD_VERSIONED_SHLIB_SUN
libgomp_version_script = -Wl,-M,libgomp.ver-sun
libgomp_version_dep = libgomp.ver-sun
libgomp.ver-sun : libgomp.ver \
		$(top_srcdir)/../contrib/make_sunver.pl \
		$(libgomp_la_OBJECTS) $(libgomp_la_LIBADD)
	perl $(top_srcdir)/../contrib/make_sunver.pl \
	  libgomp.ver \
	  $(libgomp_la_OBJECTS) $(libgomp_la_LIBADD) \
	 > $@ || (rm -f $@ ; exit 1)
endif
else
libgomp_version_script =
libgomp_version_dep =
endif

libgomp_version_info = -version-info $(libtool_VERSION)
if ENABLE_DARWIN_AT_RPATH
libgomp_darwin_rpath = -Wc,-nodefaultrpaths
libgomp_darwin_rpath += -Wl,-rpath,@loader_path
endif
libgomp_la_LDFLAGS = $(libgomp_version_info) $(libgomp_version_script) \
        $(lt_host_flags) $(libgomp_darwin_rpath)
libgomp_la_LIBADD =
libgomp_la_DEPENDENCIES = $(libgomp_version_dep)
libgomp_la_LINK = $(LINK) $(libgomp_la_LDFLAGS)

libgomp_la_SOURCES = alloc.c atomic.c barrier.c critical.c env.c error.c \
	icv.c icv-device.c iter.c iter_ull.c loop.c loop_ull.c ordered.c \
	parallel.c scope.c sections.c single.c task.c team.c work.c lock.c \
	mutex.c proc.c sem.c bar.c ptrlock.c time.c fortran.c affinity.c \
	target.c splay-tree.c libgomp-plugin.c oacc-parallel.c oacc-host.c \
	oacc-init.c oacc-mem.c oacc-async.c oacc-plugin.c oacc-cuda.c \
	priority_queue.c affinity-fmt.c teams.c allocator.c oacc-profiling.c \
	oacc-target.c target-indirect.c target-cxa-dso-dtor.c

include $(top_srcdir)/plugin/Makefrag.am

if USE_FORTRAN
libgomp_la_SOURCES += openacc.f90
endif

nodist_noinst_HEADERS = libgomp_f.h
nodist_libsubinclude_HEADERS = omp.h openacc.h acc_prof.h
if USE_FORTRAN
nodist_finclude_HEADERS = omp_lib.h omp_lib.f90 omp_lib.mod omp_lib_kinds.mod \
	openacc_lib.h openacc.f90 openacc.mod openacc_kinds.mod
endif

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

LINK = $(LIBTOOL) --tag CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=link \
	$(CCLD) $(AM_CFLAGS) $(CFLAGS) $(AM_LDFLAGS) $(LTLDFLAGS) -o $@

omp_lib_kinds.mod: omp_lib.mod
	:
openacc_kinds.mod: openacc.mod
	:
openacc.mod: openacc.lo
	:
%.mod: %.f90
	$(FC) $(FCFLAGS) -cpp -fopenmp -fsyntax-only -Wno-c-binding-type $<
fortran.lo: libgomp_f.h
fortran.o: libgomp_f.h
env.lo: libgomp_f.h
env.o: libgomp_f.h


# Automake Documentation:
# If your package has Texinfo files in many directories, you can use the
# variable TEXINFO_TEX to tell Automake where to find the canonical
# `texinfo.tex' for your package. The value of this variable should be
# the relative path from the current `Makefile.am' to `texinfo.tex'.
TEXINFO_TEX   = ../gcc/doc/include/texinfo.tex

# Defines info, dvi, pdf and html targets
MAKEINFOFLAGS = -I $(srcdir)/../gcc/doc/include
info_TEXINFOS = libgomp.texi

# AM_CONDITIONAL on configure option --generated-files-in-srcdir
if GENINSRC
STAMP_GENINSRC = stamp-geninsrc
else
STAMP_GENINSRC =
endif

# AM_CONDITIONAL on configure check ACX_CHECK_PROG_VER([MAKEINFO])
if BUILD_INFO
STAMP_BUILD_INFO = stamp-build-info
else
STAMP_BUILD_INFO =
endif


all-local: $(STAMP_GENINSRC)

stamp-geninsrc: libgomp.info
	cp -p $(top_builddir)/libgomp.info $(srcdir)/libgomp.info
	@touch $@

libgomp.info: $(STAMP_BUILD_INFO)

stamp-build-info: libgomp.texi
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir) -o libgomp.info $(srcdir)/libgomp.texi
	@touch $@


CLEANFILES = $(STAMP_GENINSRC) $(STAMP_BUILD_INFO)
MAINTAINERCLEANFILES = $(srcdir)/libgomp.info

# target overrides
-include $(tmake_file)

include $(top_srcdir)/../multilib.am
