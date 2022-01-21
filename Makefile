all:
	nim c -d:release src/main

perf: all
	perf record -e cpu-clock -g --call-graph dwarf src/main && perf report
