#include <elm/io.h>
#include <otawa/otawa.h>
#include <otawa/ipet.h>
#include <iostream> 

using namespace elm;
using namespace otawa;

int main(void) {
	try {
		WorkSpace *ws = MANAGER.load("program to analyze");
		...
	}
	catch(elm:Exception& e) {
		std::cerr << "ERROR: " << e.message() << '\n';
	}

	ipet::WCETComputation comp;
     	com.process(ws);
	cout << "the wcet is " << ipet::WCET(ws) << io::endl;

	PropList props; 
	Processor::VERBOSE(props) = true; 
	ipet::WCETComputation comp;
	comp.process(ws, props);
	cout << "the wcet is " << ipet::WCET(ws) << io::endl;

}
