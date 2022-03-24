
%: Makefile.coq

Makefile.coq: _CoqProject
	coq_makefile -f $< -o $@

tests: all
	@$(MAKE) -C tests -s clean
	@$(MAKE) -C tests -s all

-include Makefile.coq
