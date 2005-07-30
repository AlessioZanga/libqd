EPSFIGS = nine-two-sum.eps normal_sum_prod.eps qd_add.eps qd_add_proof.eps \
          qd_add_qd_d.eps qd_mul_accum.eps qd_mul_qd_d.eps quick-two-sum.eps \
          six-three-sum.eps three-sum-2.eps three-sum-3.eps three-sum.eps \
          two-prod.eps two-sum.eps 

docdir=${datadir}/doc/${PACKAGE}
dist_doc_DATA = qd.ps
dist_noinst_DATA = $(EPSFIGS) qd.tex qd.bib

qd.ps: $(EPSFIGS) qd.tex qd.bib
	latex qd.tex && bibtex qd && \
	latex qd.tex && latex qd.tex && \
	dvips -o qd.ps qd.dvi

CLEANFILES = qd.dvi qd.blg qd.bbl qd.toc qd.log qd.aux 
MAINTAINERCLEANFILES = qd.ps

