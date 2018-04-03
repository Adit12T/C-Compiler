#include "encode.h"

void spit_code(ST_ID id, TYPETAG tag, int total_dim) {
    unsigned int size = get_size_basic(tag);
    b_global_decl(st_get_id_str(id), size, total_dim* size);
    //b_alloc_int(0);
    //b_label(st_get_id_str(id));
    b_skip(total_dim * size);
}
