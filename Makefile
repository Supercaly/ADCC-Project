EBIN=ebin/

TEST_FLAGS=-DTEST

install: $(EBIN)
	cp src/ts_app.app.src ebin/ts_app.app

compile: install
	erlc -o $(EBIN) src/*.erl

$(EBIN):
	mkdir -p $@

compile_test: $(EBIN)
	erlc -o $(EBIN) $(TEST_FLAGS) src/*.erl

test: compile_test
	ct_run -dir test -pa $(EBIN)

clean:
	git clean -fxd