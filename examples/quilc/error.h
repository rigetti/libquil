#define LIBQUIL_ERROR(msg) \
  char* libquil_err; \
  get_error_message(&libquil_err); \
  printf("%s: %s\n", msg, libquil_err);
