#define LIBQUIL_ERROR(msg) \
  char* libquil_err; \
  libquil_error(&libquil_err); \
  printf("%s: %s\n", msg, libquil_err);
  
