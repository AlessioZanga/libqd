LDADD = -L$(top_builddir)/src -lqd
AM_CPPFLAGS = -I$(top_builddir) -I$(top_builddir)/include -I$(top_srcdir)/include

TESTS = qd_test pslq_test
check_PROGRAMS = qd_test pslq_test
EXTRA_PROGRAMS = qd_timer

CLEANFILES=qd_timer

qd_test_SOURCES = qd_test.cpp
pslq_test_SOURCES = pslq.h pslq_test.cpp timer.cpp timer.h
qd_timer_SOURCES = qd_timer.cpp timer.cpp timer.h
