#ifndef __MR_GMP_INT
#define __MR_GMP_INT
void* gmp_int_alloc_function(size_t);
void* gmp_int_realloc_function(void*, size_t, size_t);
void  gmp_int_free_function(void*, size_t);
#endif
