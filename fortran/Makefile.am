if HAVE_FORTRAN
AM_CPPFLAGS=-I$(top_builddir)
LDADD=-L. -lqdmod -L$(top_builddir)/src -lqd $(FCLIBS)

QDMOD=qdmodule.mod
DDMOD=ddmodule.mod

lib_LIBRARIES = libqdmod.a
libqdmod_a_SOURCES = ddmod.f90 qdmod.f90 main.cpp

pkglib_DATA = $(QDMOD) $(DDMOD)

check_PROGRAMS = fortran_test
fortran_test_SOURCES = fortran_test.f90
fortran_test_LINK=$(CXXLINK)

TESTS=fortran_test

EXTRA_PROGRAMS = quaderq quaderq2d quadgsq quadtsq quadtsq2d

quaderq_SOURCES = tquaderq.f90
nodist_quaderq_SOURCES=second.f90
quaderq_LINK=$(CXXLINK)

quaderq2d_SOURCES = tquaderq2d.f90
nodist_quaderq2d_SOURCES=second.f90
quaderq2d_LINK=$(CXXLINK)

quadgsq_SOURCES = tquadgsq.f90
nodist_quadgsq_SOURCES=second.f90
quadgsq_LINK=$(CXXLINK)

quadtsq_SOURCES = tquadtsq.f90
nodist_quadtsq_SOURCES=second.f90
quadtsq_LINK=$(CXXLINK)

quadtsq2d_SOURCES = tquadtsq2d.f90
nodist_quadtsq2d_SOURCES=second.f90
quadtsq2d_LINK=$(CXXLINK)

CLEANFILES = $(EXTRA_PROGRAMS) *.mod

demo: $(EXTRA_PROGRAMS)

endif
