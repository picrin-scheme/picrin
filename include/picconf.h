#ifndef PICCONF_H__
#define PICCONF_H__

#define PIC_ARENA_SIZE 100
#define PIC_HEAP_SIZE 8192
#define PIC_STACK_SIZE 1024
#define PIC_IREP_SIZE 256

#define DEBUG 1

#if DEBUG
# define OBJECT_CREATION_DEBUG 1
# define GC_DEBUG 1
# define VM_DEBUG 1
#endif


#endif

