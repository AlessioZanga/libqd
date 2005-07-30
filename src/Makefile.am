SRC = c_dd.cpp c_qd.cpp dd.cpp f_dd.cpp \
      f_qd.cpp fpu.cpp qd.cpp qd_const.cpp
DEBUG_SRC = bits.cpp

lib_LTLIBRARIES = libqd.la

if QD_DEBUG
libqd_la_SOURCES = $(SRC) $(DEBUG_SRC)
else
libqd_la_SOURCES = $(SRC)
endif
libqd_la_LDFLAGS = -release $(VERSION)

AM_CPPFLAGS = -I$(top_builddir) -I$(top_builddir)/include -I$(top_srcdir)/include

