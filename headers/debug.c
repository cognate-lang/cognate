#ifndef DEBUG_C
#define DEBUG_C
//#define DEBUG // Uncomment this line to enable debug output for key events:
                // (pushing objects to stack, calling functions, expanding the stack)
#define WHERE fprintf(stderr,"[LOG]%s:%d\n",__FILE__,__LINE__);
#endif
