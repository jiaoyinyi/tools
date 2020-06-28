ERLC := erlc -W
EBIN := ./ebin
INC := ./inc
SRC := ./src

SRC_FILES := $(shell find $(SRC) -type f -name "*.erl")
TARGET_FILES := $(patsubst %.erl,$(EBIN)/%.beam, $(notdir $(SRC_FILES)))

.PHONY: compile print clear

compile: $(TARGET_FILES)

# src目录
$(EBIN)/%.beam: $(SRC)/%.erl
	$(ERLC) -I $(INC) -o $(EBIN) $<
# src/cfg目录
$(EBIN)/%.beam: $(SRC)/cfg/%.erl
	$(ERLC) -I $(INC) -o $(EBIN) $<

print:
	@echo "源文件:"$(SRC_FILES)
	@echo "目标文件:"$(TARGET_FILES)

clear:
	rm $(EBIN)/*.beam
