# SOURCES := main.cpp
# # Objs are all the sources, with .cpp replaced by .o
# OBJS := $(SOURCES:.cpp=.o)
# CC=clang++
# all: t
#
# # Compile the binary 't' by calling the compiler with cflags, lflags, and any libs (if defined) and the list of objects.
# t: $(OBJS)
# 	$(CC) $(CFLAGS) -o t $(OBJS) $(LFLAGS) $(LIBS)
#
# # Get a .o from a .cpp by calling compiler with cflags and includes (if defined)
# .cpp.o:
# 	$(CC) $(CFLAGS) $(INCLUDES) -c $<
#
SOURCES := main.cpp
# Objs are all the sources, with .cpp replaced by .o
OBJS := $(SOURCES:.cpp=.o)
CC = clang++

# Set the output directory for the executable
BIN_DIR := ./bin
TARGET := $(BIN_DIR)/t

all: $(TARGET)

# Create the binary in the bin folder, ensuring the bin folder exists and is cleared first
$(TARGET): $(OBJS)
	@mkdir -p $(BIN_DIR)        # Create the bin directory if it doesn't exist
	@rm $(BIN_DIR)/*         # Clear the bin directory
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LFLAGS) $(LIBS)

# Get a .o from a .cpp by calling the compiler with cflags and includes (if defined)
.cpp.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

# Clean rule to remove object files and the bin directory
clean:
	rm -f $(OBJS)
	rm -rf $(BIN_DIR)

