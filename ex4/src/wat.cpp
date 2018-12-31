#include <stdio>
#include <stdint>



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
  &&cloc_label
}

uint16_t get_2_bytes(byte *arg) {
  return (arg[0] << 8) | arg[1];
}

int int main(int argc, char const *argv[]) {

  // TODO: Load program
  // Possibly padding it

  std::stack<int32_t> the_stack;


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
          auto tp = the_stack.pop();

          pc =  (tp != 0) ?
                &byte_program[get_2_bytes(&pc[JNZ_ARG1])] :
                pc +

        case DUP:
        dup_label:

        case SWAP:
        swap_label:

        case DROP:
        drop_label:

        case PUSH4:
        push4_label:

        case PUSH2:
        push4_label:

        case PUSH1:
        push4_label:

        case ADD:
        add_label:

        case SUB:
        sub_label:

        case MUL:
        mul_label:

        case DIV:
        div_label:

        case MOD:
        mod_label:

        case EQ:
        eq_label:

        case NE:
        ne_label:

        case LT:
        lt_label:

        case GT:
        gt_label:

        case LE:
        le_label:

        case GE:
        ge_label:

        case NOT:
        not_label:

        case AND:
        and_label:

        case OR:
        or_label:

        case INPUT:
        input_label:

        case OUTPUT:
        output_label:

        case CLOCK:
        cloc_label:

      }

  }
_end_label:
  return 0;
}
