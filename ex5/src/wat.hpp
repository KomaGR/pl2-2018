#ifndef _HPP_WAT_
#define _HPP_WAT_

// opcodes

#define HALT 0x00
#define JUMP 0x01
#define JNZ 0x02
#define DUP 0x03
#define SWAP 0x04
#define DROP 0x05
#define PUSH4 0x06
#define PUSH2 0x07
#define PUSH1 0x08
#define ADD 0x09
#define SUB 0x0A
#define MUL 0x0B
#define DIV 0x0C
#define MOD 0x0D
#define EQ 0x0E
#define NE 0x0F
#define LT 0x10
#define GT 0x11
#define LE 0x12
#define GE 0x13
#define NOT 0x14
#define AND 0x15
#define OR 0x16
#define INPUT 0x17
#define OUTPUT 0x18
#define CONS 0x30
#define HD 0x31
#define TL 0x32
#define CLOCK 0x2A

// op args

#define JUMP_ARG1 1
#define JUMP_SIZEOF 3
#define JNZ_ARG1 1
#define JNZ_SIZEOF 3
#define DUP_ARG1 1
#define DUP_SIZEOF 2
#define SWAP_ARG1 1
#define SWAP_SIZEOF 2
#define DROP_SIZEOF 1
#define PUSH4_ARG1 1
#define PUSH4_SIZEOF 5
#define PUSH2_ARG1 1
#define PUSH2_SIZEOF 3
#define PUSH1_ARG1 1
#define PUSH1_SIZEOF 2
#define ADD_SIZEOF 1
#define SUB_SIZEOF 1
#define MUL_SIZEOF 1
#define DIV_SIZEOF 1
#define MOD_SIZEOF 1
#define EQ_SIZEOF 1
#define NE_SIZEOF 1
#define LT_SIZEOF 1
#define GT_SIZEOF 1
#define LE_SIZEOF 1
#define GE_SIZEOF 1
#define NOT_SIZEOF 1
#define AND_SIZEOF 1
#define OR_SIZEOF 1
#define INPUT_SIZEOF 1
#define OUTPUT_SIZEOF 1
#define CONS_SIZEOF 1
#define HD_SIZEOF 1
#define TL_SIZEOF 1
#define CLOCK_SIZEOF 1

typedef unsigned char byte;

uint16_t get_2_bytes(byte*);
uint32_t get_4_bytes(byte*);

#endif
