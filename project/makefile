# Variables
ERLC = erlc +debug_info
SRC_DIR = src/erlang
TEST_DIR = src/test
EBIN_DIR = ebin

# Source files
SRC_FILES = $(SRC_DIR)/key_gen.erl $(SRC_DIR)/network.erl $(SRC_DIR)/leaf_set.erl \
            $(SRC_DIR)/routing.erl $(SRC_DIR)/node_actions.erl $(SRC_DIR)/node.erl $(SRC_DIR)/utils.erl \
			$(SRC_DIR)/file_handler.erl $(SRC_DIR)/pastry_actions.erl $(SRC_DIR)/web_responses.erl \
			$(SRC_DIR)/backup_actions.erl $(SRC_DIR)/controller.erl 

# Test files
TEST_FILES = $(TEST_DIR)/key_gen_test.erl $(TEST_DIR)/network_test.erl $(TEST_DIR)/leaf_set_test.erl \
             $(TEST_DIR)/routing_test.erl $(TEST_DIR)/node_actions_test.erl $(TEST_DIR)/node_test.erl \
			 $(TEST_DIR)/file_handler_test.erl $(TEST_DIR)/web_responses_test.erl $(TEST_DIR)/pastry_actions_test.erl \
			 $(SRC_DIR)/exit_test.erl 

# Compiled files
BEAM_FILES = $(SRC_FILES:$(SRC_DIR)/%.erl=$(EBIN_DIR)/%.beam)
TEST_BEAM_FILES = $(TEST_FILES:$(TEST_DIR)/%.erl=$(EBIN_DIR)/%.beam)

# Default target
all: $(BEAM_FILES) $(TEST_BEAM_FILES)

# Compile source files
$(EBIN_DIR)/utils.beam: $(SRC_DIR)/utils.erl
	$(ERLC) -o $(EBIN_DIR) $<
	

$(EBIN_DIR)/key_gen.beam: $(SRC_DIR)/key_gen.erl $(EBIN_DIR)/utils.beam 
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/network.beam: $(SRC_DIR)/network.erl 
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/leaf_set.beam: $(SRC_DIR)/leaf_set.erl $(EBIN_DIR)/key_gen.beam 
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/routing.beam: $(SRC_DIR)/routing.erl $(EBIN_DIR)/key_gen.beam $(EBIN_DIR)/network.beam $(EBIN_DIR)/utils.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/file_handler.beam: $(SRC_DIR)/file_handler.erl $(EBIN_DIR)/network.beam 
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/node_actions.beam: $(SRC_DIR)/node_actions.erl $(EBIN_DIR)/leaf_set.beam $(EBIN_DIR)/routing.beam $(EBIN_DIR)/file_handler.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/web_responses.beam: $(SRC_DIR)/web_responses.erl $(EBIN_DIR)/node_actions.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/backup_actions.beam: $(SRC_DIR)/backup_actions.erl $(EBIN_DIR)/node_actions.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/pastry_actions.beam: $(SRC_DIR)/pastry_actions.erl $(EBIN_DIR)/backup_actions.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/node.beam: $(SRC_DIR)/node.erl $(EBIN_DIR)/pastry_actions.beam $(EBIN_DIR)/web_responses.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/controller.beam: $(SRC_DIR)/controller.erl $(EBIN_DIR)/node.beam
	$(ERLC) -o $(EBIN_DIR) $<


# Compile test files
$(EBIN_DIR)/key_gen_test.beam: $(TEST_DIR)/key_gen_test.erl $(EBIN_DIR)/key_gen.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/network_test.beam: $(TEST_DIR)/network_test.erl $(EBIN_DIR)/network.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/file_handler_test.beam: $(TEST_DIR)/file_handler_test.erl $(EBIN_DIR)/file_handler.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/leaf_set_test.beam: $(TEST_DIR)/leaf_set_test.erl $(EBIN_DIR)/leaf_set.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/routing_test.beam: $(TEST_DIR)/routing_test.erl $(EBIN_DIR)/routing.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/node_actions_test.beam: $(TEST_DIR)/node_actions_test.erl $(EBIN_DIR)/node_actions.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/web_responses_test.beam: $(TEST_DIR)/web_responses_test.erl $(EBIN_DIR)/web_responses.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/pastry_actions_test.beam: $(TEST_DIR)/pastry_actions_test.erl $(EBIN_DIR)/pastry_actions.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/exit_test.beam: $(TEST_DIR)/exit_test.erl $(EBIN_DIR)/pastry_actions.beam
	$(ERLC) -o $(EBIN_DIR) $<

$(EBIN_DIR)/node_test.beam: $(TEST_DIR)/node_test.erl $(EBIN_DIR)/node.beam
	$(ERLC) -o $(EBIN_DIR) $<

# Clean target
clean:
	rm -f $(EBIN_DIR)/*.beam

.PHONY: all clean
