NAME=erlami

all: compile tests release

compile:
	./rebar compile

release:
	cd rel && ../rebar generate && cd -

node:
	(cd rel && ../rebar create-node nodeid=${NAME} && cd -)

clean:
	./rebar clean
	rm -rf rel/${NAME}

run:
	rel/${NAME}/bin/${NAME} start

runconsole:
	rel/${NAME}/bin/${NAME} console

tests:
	./rebar eunit skip_deps=true

alldev: clean all runconsole
