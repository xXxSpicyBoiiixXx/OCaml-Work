#include "stat.h" 

namespace useful { namespace stat { 

class InstCounter: public BBProcessor { 

public: 
	p::declare reg; 
	InstCounter(p::declare& r = reg): BBProcessor(r) { } 

protected: 
	virtual void processBB(WorkSpace *ws, CFG *cfg, BasicBlock *bb) { ... }
};

p::declare InstCounter::reg = p::init("useful::stat::InstCounter", Version(1,0,0))
	.base(BBProcessor::reg)
	.maker<InstCounter>()
	.provide(COUNT_FEATURE);

p::Identifier<int> INST_COUNT("userful::stat::INST_COUNT", -1);
p::feature COUNT_FEATURE("usefule::stat::COUNT_FEATURE", new Maker<InstCounter>());

}}
