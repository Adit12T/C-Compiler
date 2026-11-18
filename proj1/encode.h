#ifndef ENCODE_H
#define ENCODE_H

#include "types.h"
#include "symtab.h"
#include BACKEND_HEADER_FILE

void spit_code(ST_ID id, TYPETAG tag, int total_dim);

#endif
