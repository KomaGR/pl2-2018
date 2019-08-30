#ifndef _HPP_HEP_
#define _HPP_HEP_

#define IFADDR(_x) (((_x >> 30) & 0b11) == 0b10)
#define ADDR(_y) (_y & ~(1 << 31))

typedef unsigned char byte;

typedef struct heap_obj {
    int32_t hd;
    int32_t tl;
    uint8_t metadata;
} heap_entry;

class Heap {
private:
    std::vector<heap_entry> hp;
    std::vector<int32_t> freeList;
    std::vector<int32_t> *stk;
    int32_t upperBound;
public:
    Heap(std::vector<int32_t>*);
    virtual ~Heap ();
    int32_t add(int32_t a, int32_t b);
    int64_t get(int32_t idx, int pos);
    void bird_is_the_word(int32_t new_addr);
};
#endif
