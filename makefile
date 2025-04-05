# Project settings
EXEC = rat
SRC = Main.hs Group.hs Room.hs Solver.hs InputReader.hs OutputWriter.hs
GHC_FLAGS = -Wall -Werror -O2

# Default build target
all: $(EXEC)

# Build the executable
$(EXEC): $(SRC)
	ghc $(GHC_FLAGS) -o $(EXEC) Main.hs

# Remove compiled files and executable
clean:
	rm -f *.o *.hi $(EXEC)

# Just remove the executable (keep object files)
clean-exec:
	rm -f $(EXEC)

# Remove all compiled Haskell files except source
clean-intermediate:
	rm -f *.o *.hi

# Run the program with sample input
run:
	./$(EXEC) input/rooms.csv input/groups.csv

.PHONY: all clean clean-exec clean-intermediate run
