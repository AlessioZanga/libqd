HDR = qd/c_dd.h qd/c_qd.h qd/dd.h \
      qd/dd_inline.h qd/fpu.h qd/inline.h \
      qd/qd.h qd/qd_inline.h
DEBUG_HDR = qd/bits.h

if QD_DEBUG
nobase_include_HEADERS = $(HDR) $(DEBUG_HDR)
else
nobase_include_HEADERS = $(HDR)
endif

nobase_nodist_include_HEADERS = qd/qd_config.h

DISTCLEANFILES = qd/qd_config.h

