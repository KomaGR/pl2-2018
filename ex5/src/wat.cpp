#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstdint>
#include <algorithm>
#include <ctime>
#include <vector>

#include "wat.hpp"
#include "heep.hpp"
// #define PRINTDEBUG
#define NEXT_INSTRUCTION goto *(void *)(label_tab[*pc])
// #define NEXT_INSTRUCTION goto next_instruction

int main(int argc, char const *argv[]) {

  if (argc < 2) {
    std::cout << "Please provide input program as argument." << '\n';
    return 0;
  }

  std::ifstream input (argv[1], std::ifstream::in | std::ifstream::binary | std::ifstream::ate);
  int length = input.tellg();     // Max program is 2^16 -1 bytes

  #ifdef PRINTDEBUG
  std::cout << "I managed." << '\n';
  #endif
  byte *byte_program = new byte[length];
  input.seekg(0, std::ifstream::beg);
  input.read((char*)byte_program, length);
  input.close();

  #ifdef PRINTDEBUG
  std::cout << "I managed to read the file." << '\n';
  for (size_t i = 0; i < length; i++) {
      std::cout << std::hex << byte_program[i];
  }
  std::cout << '\n';
  #endif
  // TODO: Load program into byte_program
  // Possibly padding it

  // Initialize vm stack
  #ifdef PRINTDEBUG
  std::cout << "Initializing stack...";
  #endif
  std::vector<int32_t> the_stack;
  the_stack.reserve(100);
  #ifdef PRINTDEBUG
  std::cout << " done!" << '\n';
  #endif

  // Initialize vm heap
  #ifdef PRINTDEBUG
  std::cout << "Initializing heap...";
  #endif
  Heap hp (&the_stack);

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
  &&input_label,    // 23
  &&output_label,   // 24
  0,                // 25
  0, 0, 0, 0, 0,    // 30
  0, 0, 0, 0 ,0,    // 35
  0, 0, 0, 0, 0,    // 40
  0,
  &&clock_label,     // 42 (0x2a) aka "Thanks, I hate it"
  0, 0, 0,          // 45
  0, 0,
  &&cons_label,     // 48 (0x30)
  &&hd_label,
  &&tl_label
};

  byte *pc = &byte_program[0];
  byte opcode;

  int32_t a, b, c;
  clock_t start, end;
  double cpu_time_used;
  start = clock();
  #ifdef PRINTDEBUG
  std::cout << "Starting vm loop." << '\n';
  std::cout << '\n' << '\n';
  #endif
  while (1) {
    next_instruction:
      opcode = (int) pc[0];
      #ifdef PRINTDEBUG
      std::cout << "opcode seems to be 0x" <<  std::hex << (int)opcode << '\n';
      #endif
      switch (opcode) {
        case HALT:
        halt_label:
            // std::cout << "Halt op" << '\n';
            goto _end_label;
        case JUMP:
        jump_label:

            #ifdef PRINTDEBUG
            std::cout << "Jump op" << '\n';
            std::cout << "pc is 0x" << std::hex << pc << '\n';
            #endif
            pc = &byte_program[get_2_bytes(&pc[JUMP_ARG1])];
            #ifdef PRINTDEBUG
            std::cout << std::hex << (int) pc[1] << (int) pc[2] << '\n';
            std::cout << "Jumping to 0x" << std::hex << pc << '\n';
            #endif
            NEXT_INSTRUCTION;

        case JNZ:       // JUMP NOT ZERO
        jnz_label:

            #ifdef PRINTDEBUG
            std::cout << "JNZ op" << '\n';
            #endif
            a = the_stack.back();
            the_stack.pop_back();
            pc =  (a != 0) ?
                &byte_program[get_2_bytes(&pc[JNZ_ARG1])] :
                pc + JNZ_SIZEOF
                ;
            NEXT_INSTRUCTION;

        case DUP:
        dup_label:

            #ifdef PRINTDEBUG
            std::cout << "DUP op" << '\n';
            #endif
            the_stack.push_back(*(the_stack.rbegin() + (int) pc[DUP_ARG1]));
            pc += DUP_SIZEOF;   //2
            NEXT_INSTRUCTION;

        case SWAP:
        swap_label:

            #ifdef PRINTDEBUG
            std::cout << "SWAP op" << '\n';
            #endif
            std::iter_swap(the_stack.rbegin(), the_stack.rbegin() +  (int) pc[SWAP_ARG1]);
            pc += SWAP_SIZEOF;  //2
            NEXT_INSTRUCTION;

        case DROP:
        drop_label:

            #ifdef PRINTDEBUG
            std::cout << "DROP op" << '\n';
            #endif
            the_stack.pop_back();
            pc += DROP_SIZEOF;  //1
            NEXT_INSTRUCTION;

        case PUSH4:
        push4_label:

            #ifdef PRINTDEBUG
            std::cout << "PUSH4 op" << '\n';
            #endif
            the_stack.push_back(get_4_bytes(&pc[PUSH4_ARG1]));
            pc += PUSH4_SIZEOF;
            NEXT_INSTRUCTION;

        case PUSH2:
        push2_label:

            #ifdef PRINTDEBUG
            std::cout << "PUSH2 op" << '\n';
            #endif
            the_stack.push_back(get_2_bytes(&pc[PUSH2_ARG1]));
            pc += PUSH2_SIZEOF; //3
            NEXT_INSTRUCTION;

        case PUSH1:
        push1_label:

            #ifdef PRINTDEBUG
            std::cout << "PUSH1 op" << '\n';
            #endif
            the_stack.push_back(pc[PUSH1_ARG1]);
            pc += PUSH1_SIZEOF; //2
            NEXT_INSTRUCTION;

        case ADD:
        add_label:

            #ifdef PRINTDEBUG
            std::cout << "ADD op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back(a+b);
            pc += ADD_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case SUB:
        sub_label:

            #ifdef PRINTDEBUG
            std::cout << "SUB op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back(a-b);
            pc += SUB_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case MUL:
        mul_label:

            #ifdef PRINTDEBUG
            std::cout << "MUL op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back(a*b);
            pc += MUL_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case DIV:
        div_label:

            #ifdef PRINTDEBUG
            std::cout << "DIV op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back(a/b);
            pc += DIV_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case MOD:
        mod_label:

            #ifdef PRINTDEBUG
            std::cout << "MOD op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back(a%b);
            pc += MOD_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case EQ:
        eq_label:

            #ifdef PRINTDEBUG
            std::cout << "EQ op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a==b)?1:0);
            pc += EQ_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case NE:
        ne_label:

            #ifdef PRINTDEBUG
            std::cout << "NE op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a!=b)?1:0);
            pc += NE_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case LT:
        lt_label:

            #ifdef PRINTDEBUG
            std::cout << "LT op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a<b)?1:0);
            pc += LT_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case GT:
        gt_label:

            #ifdef PRINTDEBUG
            std::cout << "GT op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a>b)?1:0);
            pc += GT_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case LE:
        le_label:

            #ifdef PRINTDEBUG
            std::cout << "LE op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a<=b)?1:0);
            pc += LE_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case GE:
        ge_label:

            #ifdef PRINTDEBUG
            std::cout << "GE op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a>=b)?1:0);
            pc += GE_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case NOT:
        not_label:

            #ifdef PRINTDEBUG
            std::cout << "NOT op" << '\n';
            #endif
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a==0)?1:0);
            pc += NOT_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case AND:
        and_label:

            #ifdef PRINTDEBUG
            std::cout << "AND op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a!=0 && b!=0)?1:0);
            pc += AND_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case OR:
        or_label:

            #ifdef PRINTDEBUG
            std::cout << "OR op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            the_stack.push_back((a==0 && b==0)?0:1);
            pc += OR_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case INPUT:
        input_label:

            #ifdef PRINTDEBUG
            std::cout << "INPUT op" << '\n';
            #endif
            c = (int32_t) getchar();
            the_stack.push_back(c);
            pc += INPUT_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case OUTPUT:
        output_label:

            #ifdef PRINTDEBUG
            std::cout << "OUTPUT op" << '\n';
            #endif
            b = the_stack.back();
            the_stack.pop_back();
            putchar((char) b);
            // putchar('\n');
            fflush(stdout);
            pc += OUTPUT_SIZEOF;   //1
            NEXT_INSTRUCTION;

        case CONS:
        cons_label:

            b = the_stack.back();
            the_stack.pop_back();
            a = the_stack.back();
            the_stack.pop_back();
            c = hp.add(a, b);
            the_stack.push_back(c);
            pc += CONS_SIZEOF;
            NEXT_INSTRUCTION;

        case HD:
        hd_label:

            a = the_stack.back();
            the_stack.pop_back();
            b = hp.get(a, 0);
            the_stack.push_back(b);
            pc += HD_SIZEOF;
            NEXT_INSTRUCTION;

        case TL:
        tl_label:

            a = the_stack.back();
            the_stack.pop_back();
            b = hp.get(a, 1);      // Auto truncates to int32_t (fingers crossed)
            the_stack.push_back(b);
            pc += TL_SIZEOF;
            NEXT_INSTRUCTION;

        case CLOCK:
        clock_label:

            #ifdef PRINTDEBUG
            std::cout << "CLOCK op" << '\n';
            #endif
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            printf("%0.6lf\n", cpu_time_used);
            pc += CLOCK_SIZEOF;
            NEXT_INSTRUCTION;

      }

  }
_end_label:
  return 0;
}

uint16_t get_2_bytes(byte *arg) {
  return (arg[1] << 8) | arg[0];
}

uint32_t get_4_bytes(byte *arg) {
  return (arg[3] << 24) | (arg[2] << 16) | (arg[1] << 8) | arg[0];
}
