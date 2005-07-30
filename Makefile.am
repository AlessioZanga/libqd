ACLOCAL_AMFLAGS = -I m4
SUBDIRS = config src include tests fortran docs

docdir=${datadir}/doc/${PACKAGE}
dist_doc_DATA = README

changelog:
	tla changelog >ChangeLog

dist-hook:
	rm -f ${distdir}/ChangeLog && tla changelog >${distdir}/ChangeLog

pristine: maintainer-clean
	tla inventory --precious | xargs rm

fortran-demo:
	cd fortran && make demo

bin_SCRIPTS=qd-config

