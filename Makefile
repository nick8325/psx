OPTS?=

all:
	nim c -d:release ${OPTS} src/main

jollygood: 
	mkdir -p cores/psx
	nim c -d:release -o:cores/psx/psx.so --app:lib ${OPTS} -p:src src/jollygood/core

run: all
	src/main

run-jollygood: jollygood
	jollygood -c psx test.cue

perf: all
	perf record -e cpu-clock -g --call-graph dwarf src/main && perf report
