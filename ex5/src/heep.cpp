// Γλώσσες Προγραμματισμού 2 2018
// Ορέστης Καπαρουνάκης 03114057
// Άσκηση 5 - Συλλογή Σκουπιδιών
// Comments:
// Σύμφωνα με τον ορισμό της εκφώνησης τα cons cells μπορούν να
// περιέχουν cons cells. Δεν νομίζω ότι το έχω λάβει υπόψη αυτό.

#include <cstdint>
#include <vector>
#include "heep.hpp"

// #define PRINTDEBUG
#ifdef PRINTDEBUG
#include <iostream>
#endif

Heap::Heap(std::vector<int32_t> *stak) {
    stk = stak;
    hp.reserve(100);
    upperBound = hp.capacity();
}

/* Not sure if this actually frees up the memory but 
   it's only used once on main's stack frame anyway. */
Heap::~Heap() {
    hp.clear();
}

int32_t Heap::add(int32_t a, int32_t b) {
    int32_t addr;
    heap_entry ab_tuple = { .hd = a, .tl = b, .metadata = 0 };
    if (freeList.empty()) {
        hp.push_back(ab_tuple);
        addr = hp.size()-1;  // Is safe because only happens when *adding* to heap
        upperBound = hp.capacity();
        #ifdef PRINTDEBUG
        std::cout << "Created heap address: " << addr << '\n';
        #endif
    } else {
        int32_t freePos = freeList.back();
        freeList.pop_back();
        hp.at(freePos) = (ab_tuple);
        addr = freePos;
        #ifdef PRINTDEBUG
        std::cout << "Gave free address: " << addr << '\n';
        #endif
    }
    addr |= (1 << 31);       // Set ptr bit
    addr &= ~(1 << 30);      // (Not really needed. If it's 1 we're in trouble.)
    /*  Thus, pointers always start with 0b10xx. 
        This can't happen with normal ints up to 30 bits. */
    if (freeList.empty() && hp.size() >= upperBound) {
        /* Try to free up memory */
        this->bird_is_the_word(addr);
    }
    return addr;
}

int64_t Heap::get(int32_t idx, int pos) {
    int32_t addr;
    addr = ADDR(idx);   // Unset ptr bit
    if (pos == 0) {
        return hp.at(addr).hd;
    } else {
        return hp.at(addr).tl;
    }
}

// Call this function when current heap size is depleted
void Heap::bird_is_the_word(int32_t new_addr) {
    /* Other side most definately has access to *addr after we return */
    /* ## MARK ## */
    #ifdef PRINTDEBUG
    std::cout << "## Running GC!\n\n";
    #endif

    // For each stack element if heap flag is set mark heap thing
    std::vector<int32_t> to_check;
    to_check.push_back(new_addr);
    for (auto t = stk->begin(); t != stk->end(); ++t) {
        /* DFS from *t */
        if (IFADDR(*t)) {
            to_check.push_back(*t);     // A pointer to the heap
        }    
    } 

    while (!to_check.empty()) {
        int32_t curr_entry = to_check.back();
        to_check.pop_back();
        
        int32_t addr = ADDR(curr_entry);   // Calculate corresponding position at heap
            
        if (hp.at(addr).metadata > 0) {
            /* Already marked. 
                Also: Cycle detected. */
            continue;
        }
        #ifdef PRINTDEBUG
        std::cout << "#GC: Mark 0a" << addr << '\n';
        #endif

        hp.at(addr).metadata = 0b10000000;  // Mark it

        if (IFADDR(hp.at(addr).hd) &&
            hp.at(ADDR(hp.at(addr).hd)).metadata == 0) {
            to_check.push_back(hp.at(addr).hd);
            hp.at(addr).metadata |= 0b01000000;
        }
        if (IFADDR(hp.at(addr).tl) &&
            hp.at(ADDR(hp.at(addr).tl)).metadata == 0) {
            to_check.push_back(hp.at(addr).tl);
            hp.at(addr).metadata |= 0b00100000;
        }
    }

    /* ## SWEEP ## */
    for (int32_t t = 0; t != hp.size(); ++t) {        
        if (hp.at(t).metadata == 0) {
            #ifdef PRINTDEBUG
            std::cout << "#GC: Free 0a" << t << '\n';
            #endif
            freeList.push_back(t);
        }
    }
    /* Reset metadata */
    for (auto t = hp.begin(); t != hp.end(); ++t) {
        t->metadata = 0;
    }
}

