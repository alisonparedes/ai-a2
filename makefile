all:	vacroplan

vacroplan: 	vacroplan.ml fheap
	ocamlopt -c vacroplan.ml
	ocamlopt -o run.sh fheap.cmx vacroplan.cmx

debug: 	vacroplan.ml
	ocamlc -c -g fheap.ml
	ocamlc -c -g vacroplan.ml
	ocamlc -g -o run.sh fheap.cmo vacroplan.cmo

test_vacroplan:	vacroplan testRunner.ml testVacroplan.ml
	ocamlopt -c testRunner.ml
	ocamlopt -c testVacroplan.ml
	ocamlopt -o test_vacroplan fheap.cmx vacroplan.cmx testRunner.cmx testVacroplan.cmx

debugtest:	debug testRunner.ml testVacroplan.ml
	ocamlc -c -g testRunner.ml
	ocamlc -c -g testVacroplan.ml
	ocamlc -g -o test fheap.cmo vacroplan.cmo testRunner.cmo testVacroplan.cmo

fheap:	fheap.ml
	ocamlopt -c fheap.ml

test_fheap:	fheap vacroplan testRunner.ml testFheap.ml
		ocamlopt -c testRunner.ml
		ocamlopt -c testFheap.ml
		ocamlopt -o test_fheap fheap.cmx vacroplan.cmx testRunner.cmx testFheap.cmx
