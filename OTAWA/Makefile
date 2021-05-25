MODULES		= lp_solve ppc 
PROGRAM		= ipet 
SOURCES		= ipet.cpp

# Configuration 
FLAGS		= $(shell otawa-config --cflags $(MODULES))
DATADIR		= $(shell otawa-config --data $(MODULES))
CXXFLAGS	= $(FLAGS) -DDATA_DIR="$(DATADIR)"
LDLIBS		= $(shell otawa-config --libs $(MODULES))
LDFLAGS		= -dlopen force
CXX 		= g++
LD 		= libtool --mode=link --tag=CXX g++ 

# Derivated
OBJECTS		= $(SOURCES:.cpp=.o)
DEPS		= $(addprefix .deps,$(SOURCES:.cpp=.d))

# Rules 
all: .deps $(PROGRAM)

$(PROGRAM): $(OBJECTS)
	$(LD) -o $@ $^ $(LDFLAGS) $(LDLIBS)

clean:
	rm -rf *.o $(PROGRAM) *~ core.* *.lo .libs .deps

.deps:
	mkdir .deps 

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -MM -MF .deps/$*.d -c $<
	$(CXX) $(CXXFLAGS) -c $< -o $@

-include $(DEPS)
