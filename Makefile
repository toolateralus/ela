COMPILER := clang++
COMPILER_FLAGS := -std=c++23 -g -DMAX_NUM_TYPES=2000
LD_FLAGS := 
OBJ_DIR := objs
BIN_DIR := bin

SRCS := $(filter-out output.cpp, $(wildcard *.cpp))
OBJS := $(patsubst %.cpp,$(OBJ_DIR)/%.o,$(SRCS))
all: directories ela

directories:
	mkdir -p $(OBJ_DIR) $(BIN_DIR)

ela: $(OBJS)
	$(COMPILER) $(COMPILER_FLAGS) -o $(BIN_DIR)/$@ $^ $(LD_FLAGS)

$(OBJ_DIR)/%.o: %.cpp
	mkdir -p $(@D)
	$(COMPILER) $(COMPILER_FLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

run: all ela
	./$(BIN_DIR)/ela