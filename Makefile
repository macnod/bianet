TEST_NEURON_FILE="$(HOME)/common-lisp/bianet/test-neuron.lisp"
TEST_NETWORK_FILE="$(HOME)/common-lisp/bianet/test-network.lisp"
TEST_API_FILE="$(HOME)/common-lisp/bianet/test-api.lisp"
# TEST_WORK_FILE="$(HOME)/common-lisp/bianet/test-work.lisp"
LISP=/usr/bin/sbcl
# Reporter can be list, dot, tap, or fiveam.
REPORTER=list
test-neuron:
	$(LISP) --eval "(ql:quickload :prove)" \
	  --eval "(require :prove)" \
	  --eval "(prove:run #P\"$(TEST_NEURON_FILE)\" :reporter :$(REPORTER))" \
	  --non-interactive

test-network:
	$(LISP) --eval "(ql:quickload :prove)" \
	  --eval "(require :prove)" \
	  --eval "(prove:run #P\"$(TEST_NETWORK_FILE)\" :reporter :$(REPORTER))" \
	  --non-interactive

test-api:
	$(LISP) --eval "(ql:quickload :prove)" \
		--eval "(require :prove)" \
		--eval "(prove:run #P\"$(TEST_API_FILE)\" :reporter :$(REPORTER))" \
	  --non-interactive

test:	test-neuron	test-network

# work:
# 	$(LISP) --eval "(ql:quickload :prove)" \
# 	  --eval "(require :prove)" \
# 	  --eval "(prove:run #P\"$(TEST_WORK_FILE)\" :reporter :$(REPORTER))" \
# 	  --non-interactive
