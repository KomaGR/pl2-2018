#include <cstdint>
#include <vector>
#include "heep.hpp"


Heap::Heap(std::vector<int32_t> *stak) {
    stk = stak;
    hp.reserve(100);
}

// Not sure if this actually frees up the
// memory but it's only used once on main's
// stack frame anyway.
Heap::~Heap() {
    hp.clear();
}

int32_t Heap::add(int32_t a, int32_t b) {
    hp.push_back((((int64_t) a) << 32) | b);
    return hp.size()-1;  // Is safe because only happens when *adding* to heap
}

int64_t Heap::get(int32_t idx) {
    return hp.at(idx);
}

void Heap::bird_is_the_word() {
    // For each stack element if heap flag is set mark heap thing
    for (auto t = stk->begin(); t != stk->end(); ++t) {
        if (*t & ~PTRBIT == PTRBIT | ~PTRBIT) { // WAIT WHAT ABOUT THE SIGN
            /* is address for the heap */
        }
    }
}
