# Builds all three stages of the compiler; `make test` runs the test programs.
STAGES = proj1 proj2 proj3

.PHONY: all test clean $(STAGES)

all: $(STAGES)

$(STAGES):
	$(MAKE) -C $@

test: all
	./tests/run.sh

clean:
	for s in $(STAGES); do $(MAKE) -C $$s clean; done
