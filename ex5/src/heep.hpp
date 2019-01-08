#ifndef _HPP_HEP_
#define _HPP_HEP_

#define PTRBIT 0x8000000000000000

typedef unsigned char byte;

class Heap {
private:
    std::vector<int64_t> hp;
    std::vector<int32_t> *stk;
public:
    Heap(std::vector<int32_t>*);
    virtual ~Heap ();
    int32_t add(int32_t a, int32_t b);
    int64_t get(int32_t idx);
    void bird_is_the_word(void);
};
#endif
