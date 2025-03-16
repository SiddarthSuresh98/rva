BIN := rva
LISP := sbcl

.PHONY: all run clean

all:
	$(LISP) --non-interactive \
		--eval '(require "asdf")' \
		--eval '(asdf:load-asd (merge-pathnames "rva.asd" (uiop:getcwd)))' \
		--eval '(asdf:make :rva)'

run:
	@ ./bin/$(BIN)

test:
	$(LISP) --non-interactive \
		--eval '(require "asdf")' \
	        --eval '(asdf:load-asd (merge-pathnames "rva.asd" (uiop:getcwd)))' \
		--eval '(asdf:test-system :rva/tests)'

clean:
	rm -f ./bin/$(BIN)
