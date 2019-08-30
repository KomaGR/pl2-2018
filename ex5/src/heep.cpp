#include <cstdint>
#include <vector>
#include "heep.hpp"


Heap::Heap(std::vector<int32_t> *stak) {
    stk = stak;
    hp.reserve(100);
    upperBound = 100;
}

/* Not sure if this actually frees up the memory but 
   it's only used once on main's stack frame anyway. */
Heap::~Heap() {
    hp.clear();
}

int32_t Heap::add(int32_t a, int32_t b) {
    int32_t addr;
    heap_entry ab_tuple = { .hd = a, .tl = b };
    if (hp.size() >= upperBound-1) {
        /* Try to free up memory */
        // this.bird_is_the_word();
    }
    if (freeList.empty()) {
        hp.push_back(ab_tuple);
        addr = hp.size()-1;  // Is safe because only happens when *adding* to heap
    } else {
        int32_t freePos = freeList.back();
        freeList.pop_back();
        hp.at(freePos) = (ab_tuple);
        addr = freePos;
    }
    addr |= (1UL << 31);       // Set ptr bit
    addr &= ~(1UL << 30);      // (Not really needed. If it's 1 we're in trouble.)
    /*  Thus, pointers always start with 0b10xx. 
        This can't happen with normal ints up to 30 bits. */
    return addr;
}

int64_t Heap::get(int32_t idx, int pos) {
    int32_t addr;
    addr = idx & ~(1UL << 31);   // Unset ptr bit
    if (pos == 0) {
        return hp.at(addr).hd;
    } else {
        return hp.at(addr).tl;
    }
}

// Call this function when current heap size is depleted
void Heap::bird_is_the_word() {
    // For each stack element if heap flag is set mark heap thing
    for (auto t = stk->begin(); t != stk->end(); ++t) {
        int32_t bits = (*t >> 30);
        if (bits == 0b10) {
            /* Mark & DFS(*t) */
            hp.at(*t);
        }
    }
}

