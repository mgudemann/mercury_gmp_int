#include <stdlib.h>

extern	void	*MR_GC_malloc(size_t num_bytes);
extern	void	*MR_GC_realloc(void *ptr, size_t num_bytes);
extern	void	*GC_free(void *ptr);

void *
gmp_int_alloc_function(size_t size)
{
  return MR_GC_malloc(size);
}

void *
gmp_int_realloc_function(void* ptr, size_t size, size_t new_size)
{
  return MR_GC_realloc(ptr, new_size);
}

void
gmp_int_free_function(void* ptr, size_t size)
{
  GC_free(ptr);
}
