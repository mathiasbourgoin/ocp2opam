all:
	ocp-build -init || ocp-build init

install: 
	ocp-build -install || ocp-build install

uninstall:
	ocp-build -uninstall || ocp-build uninstall
