#include <rel_sqlite3_extension.h>

extern int sqlite3_decimal_init
  (sqlite3 *, char **, const sqlite3_api_routines *);

CAMLprim value fin_sqlite_decimal_extension (value unit)
{
  (void) unit;
  return rel_sqlite3_extension_make (sqlite3_decimal_init);
}
