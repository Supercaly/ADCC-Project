BIN=bin
TS_BIN=ts_app/ebin
PROFLIB_BIN=proflib_app/ebin
EXPERIMENTS_BIN=experiments_app/ebin

all: ts proflib experiments 

ts: $(BIN)
	mkdir -p $(BIN)/$(TS_BIN)
	erlc -o $(BIN)/$(TS_BIN) apps/ts/src/*.erl
	cp apps/ts/src/ts_app.app.src $(BIN)/$(TS_BIN)/ts_app.app

proflib: $(BIN)
	mkdir -p $(BIN)/$(PROFLIB_BIN)
	erlc -o $(BIN)/$(PROFLIB_BIN) apps/proflib/src/*.erl
	cp apps/proflib/src/proflib_app.app.src $(BIN)/$(PROFLIB_BIN)/proflib_app.app

experiments: $(BIN)
	mkdir -p $(BIN)/$(EXPERIMENTS_BIN)
	erlc -o $(BIN)/$(EXPERIMENTS_BIN) apps/experiments/src/*.erl
	cp apps/experiments/src/experiments.app.src $(BIN)/$(EXPERIMENTS_BIN)/experiments.app

$(BIN):
	mkdir -p $@