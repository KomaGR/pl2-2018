#include <stdio>
#include <stdint>
#include <algorithm>

// opcodes
{
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
}
// op args
{
#define JUMP_ARG1 1
#define JUMP_SIZEOF 3
#define JNZ_ARG1 1
#define JNZ_SIZEOF 3
}


#define NEXT_INSTRUCTION goto *(void *)(label_tab[*pc])

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
}

uint16_t get_2_bytes(byte *arg) {
  return (arg[0] << 8) | arg[1];
}

int int main(int argc, char const *argv[]) {

  byte *byte_program;
  // TODO: Load program into byte_program
  // Possibly padding it

  // Initialize vm stack
  std::vector<int32_t> the_stack();
  the_stack.reserve(100);


  byte *pc = &byte_program[0]
  byte opcode;

  while (TRUE) {
    next_instruction:
      opcode = pc[0];
      switch (opcode) {
        case HALT:
        halt_label:
          goto _end_label;
        case JUMP:
        jump_label:
          pc = &byte_program[get_2_bytes(&pc[JUMP_ARG1])]
          NEXT_INSTRUCTION;
        case JNZ:       // JUMP NOT ZERO
        jnz_label:
          auto tp = the_stack.pop_back();

          pc =  (tp != 0) ?
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

        case PUSH2:
        push4_label:

          the_stack.push_back(get_2_bytes(&pc[PUSH2_ARG1]));
          pc += PUSH2_SIZEOF; //3
          NEXT_INSTRUCTION;

        case PUSH1:
        push4_label:

          the_stack.push_back(pc[PUSH1_ARG1]);
          pc += PUSH1_SIZEOF; //2
          NEXT_INSTRUCTION;

        case ADD:
        add_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back(a+b);
          pc += ADD_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case SUB:
        sub_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back(a-b);
          pc += SUB_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case MUL:
        mul_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back(a*b);
          pc += MUL_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case DIV:
        div_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back(a/b);
          pc += DIV_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case MOD:
        mod_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back(a%b);
          pc += MOD_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case EQ:
        eq_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back((a==b)?1:0);
          pc += EQ_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case NE:
        ne_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back((a!=b)?1:0);
          pc += NE_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case LT:
        lt_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back((a<b)?1:0);
          pc += LT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case GT:
        gt_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back((a>b)?1:0);
          pc += GT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case LE:
        le_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back((a<=b)?1:0);
          pc += LE_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case GE:
        ge_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back((a>=b)?1:0);
          pc += GE_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case NOT:
        not_label:

          auto a = the_stack.pop_back();
          the_stack.push_back((a==0)?1:0);
          pc += NOT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case AND:
        and_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
          the_stack.push_back((a!=0 && b!=0)?1:0);
          pc += AND_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case OR:
        or_label:

          auto b = the_stack.pop_back();
          auto a = the_stack.pop_back();
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

          auto c = the_stack.pop_back();
          putchar(c);
          pc += OUTPUT_SIZEOF;   //1
          NEXT_INSTRUCTION;

        case CLOCK:
        clock_label:

      }

  }
_end_label:
  return 0;
}
