all: build

build :
	ocp-build -init || ocp-build init

clean : 
	rm -f *~ 
	ocp-build -init
	ocp-build clean
install : 
	ocp-build -init
	ocp-build install

uninstall :
	ocp-build -init
	ocp-build -uninstall || ocp-build uninstall || ocamlfind remove ocp2opam
