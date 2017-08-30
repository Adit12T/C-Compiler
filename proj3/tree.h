#ifndef TREE_H
#define TREE_H

#include "symtab.h"
#include "types.h"
#include "bucket.h"

typedef enum {
	VAL_INT, VAL_DOUBLE, VAL_ST_ID
} VAL_TAG;

typedef struct val {
	VAL_TAG tag;
	union {
		int int_val;
		double real_val;
		ST_ID st_id_val;
	} v;
} VAL;

typedef enum {ID_DECL, FUNC_DECL, REF_DECL, PTR_DECL, ARR_DECL} DECL_TAG;

typedef struct decl_nd {
	ST_ID id;
	DECL_TAG type;
	int arr_size;
	PARAM_LIST params;
	struct decl_nd * next;
} DECL_ND, *DECL_ND_PTR;

typedef enum {LEAF, UNARY, BINARY} EXPR_TYPE;

typedef enum {ERROR_OP, NO_OP, FUNC_OP, PAREN_OP, DEREF_OP, CONV_OP, ASSIGN_OP, 
	PLUS_OP, MUL_OP, DIV_OP, 
	EQL_OP, NEQL_OP,
	MINUS_OP, NEGATE_OP, MOD_OP, LESS_OP, GREATER_OP} OP_TYPE;

typedef struct expr_node {
	EXPR_TYPE expr_type;
	OP_TYPE op_type;
	TYPE type;
	TYPE right_type;
	VAL val;	
	VAL_TAG tag;
	struct expr_node * left;
	struct expr_node * right;
} EXPR_ND, *EXPR_ND_PTR;

typedef enum {NOT_SWITCH, IS_SWITCH} CONTROL_TAG;

typedef struct case_pair {
	int val;
	char *label;
	struct case_pair * next;
} CASE_PAIR, *CASE_PAIR_PTR;

typedef struct cntrl_node {
	CONTROL_TAG tag;
	char * exit_label;
	char * default_label;
	CASE_PAIR_PTR pairs;
	struct cntrl_node * next;
} CONTROL_ND, *CONTROL_ND_PTR;

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

extern EXPR_ND_PTR exp_mk_real(VAL val);
extern EXPR_ND_PTR exp_mk_int(VAL val);
extern EXPR_ND_PTR exp_mk_var(VAL val);
extern EXPR_ND_PTR exp_mk_paren(EXPR_ND_PTR expr);
extern EXPR_ND_PTR exp_mk_assign(EXPR_ND_PTR left, EXPR_ND_PTR right);
extern EXPR_ND_PTR exp_mk_binary_op(OP_TYPE op_type, EXPR_ND_PTR left, EXPR_ND_PTR right);
extern EXPR_ND_PTR mk_exp_unary_op(OP_TYPE op_type, EXPR_ND_PTR expr);
extern EXPR_ND_PTR exp_mk_func_op(VAL val);

extern CONTROL_ND_PTR push_to_control_stack(CONTROL_ND_PTR top, CONTROL_TAG tag, char *label);
extern CONTROL_ND_PTR pop_from_control_stack(CONTROL_ND_PTR top);
extern char * peek_control_stack(CONTROL_ND_PTR top);
extern CONTROL_ND_PTR get_switch_closest_to_top(CONTROL_ND_PTR top);

#endif
