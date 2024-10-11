COMPILER := clang++

# MAX_NUM_TYPES should not be a compile time constant, it should be approximated or something.
# However, 2000 is pretty reasonable, most programs wont exceed triple digits at all.
COMPILER_FLAGS := -std=c++23 -g -DMAX_NUM_TYPES=2000 -Iinclude
LD_FLAGS := 
OBJ_DIR := objs
BIN_DIR := bin

SRCS := $(filter-out output.cpp, $(wildcard src/*.cpp))
OBJS := $(patsubst src/%.cpp,$(OBJ_DIR)/%.o,$(SRCS))

all: 
	$(MAKE) directories 
	$(MAKE) ela -j24

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
			
release: clean
	$(MAKE) directories
	
	$(MAKE) ela -j24 COMPILER_FLAGS="-std=c++23 -O3 -flto -DMAX_NUM_TYPES=2000 -Iinclude -fno-exceptions" LD_FLAGS="-flto -s"