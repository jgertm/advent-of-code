all:
	rm aoc
	cp aoc.head aoc
	lein uberjar
	cat target/uberjar/advent-of-code-0.1.0-SNAPSHOT-standalone.jar >> aoc
	chmod +x aoc
