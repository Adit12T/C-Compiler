#ifndef TREE_H
#define TREE_H

#include "symtab.h"
#include "types.h"

typedef enum {ID_DECL, FUNC_DECL, REF_DECL, PTR_DECL, ARR_DECL} DECL_TAG;

typedef struct decl_nd {
	ST_ID id;
	DECL_TAG type;
	int arr_size;
	PARAM_LIST params;
	struct decl_nd * next;
} DECL_ND, *DECL_ND_PTR;

/* Insert ID node */
extern DECL_ND_PTR tr_insert_id(ST_ID id);

/* Insert array node. Size required */
extern DECL_ND_PTR tr_insert_array(DECL_ND_PTR head, int size);

/* Insert pointer node */
extern DECL_ND_PTR tr_insert_ptr(DECL_ND_PTR head);

/* Insert ref node */
extern DECL_ND_PTR tr_insert_ref(DECL_ND_PTR head);

/* Insert func node --delete*/
extern DECL_ND_PTR tr_insert_func(DECL_ND_PTR head);

/* Insert func node */
extern DECL_ND_PTR tr_insert_func_params(DECL_ND_PTR head, PARAM_LIST params);

/* free tree nodes */
extern void tr_free_tree(DECL_ND_PTR head);

#endif
