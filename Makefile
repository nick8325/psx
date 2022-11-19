OPTS?=

all:
	nim c -d:release ${OPTS} src/main

run: all
	src/main

perf: all
	perf record -e cpu-clock -g --call-graph dwarf src/main && perf report
