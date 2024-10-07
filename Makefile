COMPILER := clang++
COMPILER_FLAGS := -std=c++23 -g -DMAX_NUM_TYPES=2000 -Iinclude
LD_FLAGS := 
OBJ_DIR := objs
BIN_DIR := bin

SRCS := $(filter-out output.cpp, $(wildcard src/*.cpp))
OBJS := $(patsubst src/%.cpp,$(OBJ_DIR)/%.o,$(SRCS))
all: directories ela

directories:
	mkdir -p $(OBJ_DIR) $(BIN_DIR)

ela: $(OBJS)
	$(COMPILER) $(COMPILER_FLAGS) -o $(BIN_DIR)/$@ $^ $(LD_FLAGS)

$(OBJ_DIR)/%.o: src/%.cpp
	mkdir -p $(@D)
	$(COMPILER) $(COMPILER_FLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

run: all ela
	./$(BIN_DIR)/ela