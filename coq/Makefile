all: Makefile.coq
	@$(MAKE) -f Makefile.coq all

Makefile.coq: _CoqProject
	coq_makefile -f $< -o $@

clean: Makefile.coq
	@$(MAKE) -f Makefile.coq clean

.PHONY: all clean
