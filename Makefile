.PHONY: all
all: clean test run

.PHONY: clean
clean:
	rm -Rf dist/*

.PHONY: run
run: uberjar
	java -jar dist/${CONTAINER_NAME}.jar

.PHONY: test
test:
	clj -Atest

.PHONY: uberjar 
uberjar: 
	clj -Auberjar
