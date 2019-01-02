#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstdint>
#include <algorithm>
#include <vector>

typedef unsigned char byte;
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
#define CLOCK_SIZEOF 1

#define NEXT_INSTRUCTION goto *(void *)(label_tab[*pc])


uint16_t get_2_bytes(byte *arg) {
  return (arg[0] << 8) | arg[1];
}

uint32_t get_4_bytes(byte *arg) {
  return (arg[0] << 24) | (arg[1] << 16) | (arg[2] << 8) | arg[3];
}

int main(int argc, char const *argv[]) {

  if (argc < 2) {
    std::cout << "Please provide input program as argument." << '\n';
    return 0;
  }

  std::ifstream input (argv[1], std::ifstream::in | std::ifstream::binary | std::ifstream::ate);
  // std::ifstream::pos_type pos =
  int length = input.tellg();     // Max program is 2^16 -1 bytes

  byte *byte_program = new byte[length];
  input.seekg(0, std::ifstream::beg);
  input.read(byte_program, length);
  input.close();

  // TODO: Load program into byte_program
  // Possibly padding it

  // Initialize vm stack
  std::vector<int32_t> the_stack;
  the_stack.reserve(100);


  static void *label_tab[] = {
  &&halt_label,
  &&jump_label,
  &&jnz_label,
  &&dup_label,
  &&swap_label,
  &&drop_label,
  &&push4_label,
  &&push2_label,
  &&push1_label,
  &&add_label,
  &&sub_label,
  &&mul_label,
  &&div_label,
  &&mod_label,
  &&eq_label,
  &&ne_label,
  &&lt_label,
  &&gt_label,
  &&le_label,
  &&ge_label,
  &&not_label,
  &&and_label,
  &&or_label,
  &&input_label,
  &&output_label,
  &&clock_label
};

  byte *pc = &byte_program[0];
  byte opcode;

  int32_t a, b;

  while (1) {
    next_instruction:
      opcode = pc[0];
      switch (opcode) {
        case HALT:
        halt_label:
          goto _end_label;
        case JUMP:
        jump_label:

          pc = &byte_program[get_2_bytes(&pc[JUMP_ARG1])];
          NEXT_INSTRUCTION;

        case JNZ:       // JUMP NOT ZERO
        jnz_label:

          a = the_stack.back();
          the_stack.pop_back();
          pc =  (a != 0) ?
                &byte_program[get_2_bytes(&pc[JNZ_ARG1])] :
                pc + JNZ_SIZEOF
                ;
          NEXT_INSTRUCTION;

        case DUP:
        dup_label:

          the_stack.push_back(the_stack.at(the_stack.rbegin() + (int) pc[DUP_ARG1]));
          pc += DUP_SIZEOF;   //2
          NEXT_INSTRUCTION;

        case SWAP:
        swap_label:

          std::iter_swap(the_stack.rbegin(), the_stack.rbegin() +  (int) pc[SWAP_ARG1]);
          pc += SWAP_SIZEOF;  //2
          NEXT_INSTRUCTION;

        case DROP:
        drop_label:

          the_stack.pop_back();
          pc += DROP_SIZEOF;  //1
          NEXT_INSTRUCTION;

        case PUSH4:
        push4_label:

          // TODO
          the_stack.push_back(get_4_bytes(&pc[PUSH4_ARG1]));
          pc += PUSH4_SIZEOF;
          NEXT_INSTRUCTION;

        case PUSH2:
        push2_label:

          the_stack.push_back(get_2_bytes(&pc[PUSH2_ARG1]));
          pc += PUSH2_SIZEOF; //3
          NEXT_INSTRUCTION;

        case PUSH1:
        push1_label:

          the_stack.push_back(pc[PUSH1_ARG1]);
          pc += PUSH1_SIZEOF; //2
          NEXT_INSTRUCTION;

        case ADD:
        add_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back(a+b);
          pc += ADD_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case SUB:
        sub_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back(a-b);
          pc += SUB_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case MUL:
        mul_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back(a*b);
          pc += MUL_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case DIV:
        div_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back(a/b);
          pc += DIV_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case MOD:
        mod_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back(a%b);
          pc += MOD_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case EQ:
        eq_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a==b)?1:0);
          pc += EQ_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case NE:
        ne_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a!=b)?1:0);
          pc += NE_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case LT:
        lt_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a<b)?1:0);
          pc += LT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case GT:
        gt_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a>b)?1:0);
          pc += GT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case LE:
        le_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a<=b)?1:0);
          pc += LE_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case GE:
        ge_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a>=b)?1:0);
          pc += GE_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case NOT:
        not_label:

          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a==0)?1:0);
          pc += NOT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case AND:
        and_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a!=0 && b!=0)?1:0);
          pc += AND_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case OR:
        or_label:

          b = the_stack.back();
          the_stack.pop_back();
          a = the_stack.back();
          the_stack.pop_back();
          the_stack.push_back((a==0 && b==0)?0:1);
          pc += OR_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case INPUT:
        input_label:

          auto c = getchar();
          the_stack.push_back(c);
          pc += INPUT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case OUTPUT:
        output_label:

          b = the_stack.back();
          the_stack.pop_back();
          putchar(b);
          pc += OUTPUT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case CLOCK:
        clock_label:
          goto _end_label;

      }

  }
_end_label:
  return 0;
}
