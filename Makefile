CWD=$(shell pwd)
NAME=$(shell basename ${CWD})

all: clean compile edoc release
	./rebar compile

edoc:
	./rebar doc

compile:
	./rebar compile

test: compile
	./rebar eunit skip_deps=true

release: test
	(cd rel && ../rebar generate && cd -)

node:
	(cd rel && ../rebar create-node nodeid=${NAME} && cd -)

clean:
	./rebar clean
	rm -rf rel/${NAME}

run:
	rel/${NAME}/bin/${NAME} start

runconsole:
	rel/${NAME}/bin/${NAME} console

alldev: clean all runconsole
