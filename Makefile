TESTS=$(wildcard Test/*.lua)

test: $(TESTS)
	for i in $^; do echo $$i; lua ./$$i || exit 1; echo; done

.PHONY: test
