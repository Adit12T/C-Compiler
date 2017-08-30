#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "tree.h"

DECL_ND_PTR tr_insert_id(ST_ID id) {
	DECL_ND_PTR head = (DECL_ND_PTR) malloc (sizeof(DECL_ND));	
	head->id = id;
	head->type = ID_DECL;
	head->next = NULL;
	return head;
}

DECL_ND_PTR tr_insert_func(DECL_ND_PTR head) {
	DECL_ND_PTR new_head = (DECL_ND_PTR) malloc (sizeof(DECL_ND));	
	new_head->type = FUNC_DECL;
	new_head->params = NULL;
	new_head->next = head;
	return new_head;
}

DECL_ND_PTR tr_insert_func_params(DECL_ND_PTR head, PARAM_LIST params) {
	DECL_ND_PTR new_head = (DECL_ND_PTR) malloc (sizeof(DECL_ND));	
	new_head->type = FUNC_DECL;
	new_head->params = params;
	new_head->next = head;
	return new_head;
}

DECL_ND_PTR tr_insert_ptr(DECL_ND_PTR head) {
	DECL_ND_PTR new_head = (DECL_ND_PTR) malloc (sizeof(DECL_ND));	
	new_head->type = PTR_DECL;
	new_head->next = head;
	return new_head;
}

DECL_ND_PTR tr_insert_ref(DECL_ND_PTR head) {
	DECL_ND_PTR new_head = (DECL_ND_PTR) malloc (sizeof(DECL_ND));	
	new_head->type = REF_DECL;
	new_head->next = head;
	return new_head;
}

DECL_ND_PTR tr_insert_array(DECL_ND_PTR head, int size) {
	DECL_ND_PTR new_head = (DECL_ND_PTR) malloc (sizeof(DECL_ND));	
	new_head->type = ARR_DECL;
	new_head->arr_size = size;
	new_head->next = head;
	return new_head;
}

void tr_free_tree(DECL_ND_PTR head) {
	if (head == NULL) {
		return;
	}
	while (head !=  NULL) {
		DECL_ND_PTR head1 = head->next;
		head->next = NULL;
		free(head);
		head = head1;
	}
}

EXPR_ND_PTR conv_float_to_double(EXPR_ND_PTR expr) {
	if (ty_query(expr->type) == TYFLOAT) {
		EXPR_ND_PTR conv_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
		conv_node->expr_type = UNARY;
		conv_node->op_type = CONV_OP;
		conv_node->type = build_base(update_bucket(NULL, DOUBLE_SPEC, NULL));
		conv_node->right_type = expr->type;
		conv_node->left = expr;
		conv_node->right = NULL;
		return conv_node;
	}
	return expr;
}

EXPR_ND_PTR exp_mk_binary_op(OP_TYPE op_type, EXPR_ND_PTR left, EXPR_ND_PTR right) {
	left = conv_float_to_double(left);
	right = conv_float_to_double(right);

	PARAMSTYLE pstyle;
	PARAM_LIST plist;

	TYPE ltype = left->type;
	TYPETAG ltag = ty_query(ltype);
	if (ltag == TYFUNC) {
		ltype = ty_query_func(ltype, &pstyle, &plist);
		ltag = ty_query(ltype);
	}
	TYPE rtype = right->type;
	TYPETAG rtag = ty_query(rtype);
	if (rtag == TYFUNC) {
		rtype = ty_query_func(rtype, &pstyle, &plist);
		rtag = ty_query(rtype);
	}
	int flag = -1;
	if (get_size_basic(ltag) < get_size_basic(rtag)) {
		flag = 1;
	}
	if (get_size_basic(ltag) > get_size_basic(rtag)) {
		flag = 0;
	}
	if (left->op_type == NO_OP && left->expr_type == LEAF 
				&& right->op_type == NO_OP && right->expr_type == LEAF) {
		EXPR_ND_PTR fold_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
		fold_node->expr_type = LEAF;
		fold_node->op_type = NO_OP;
		fold_node->left = NULL;
		fold_node->right = NULL;
		fold_node->type = left->type;
		if(flag == 1) {
			fold_node->type = right->type;
		}
		VAL val;
		val.tag = VAL_INT;
		if (ty_query(fold_node->type) == TYDOUBLE) {
			val.tag = VAL_DOUBLE;
		}
		fold_node->tag = val.tag;
		switch(op_type) {
			case PLUS_OP:
				if (val.tag == VAL_INT) {
					val.v.int_val = left->val.v.int_val + right->val.v.int_val;
				}
				if (val.tag == VAL_DOUBLE) {
					val.v.real_val = 0;
					if (left->tag == VAL_DOUBLE) {
						val.v.real_val += left->val.v.real_val;
					}
					else {
						val.v.real_val += left->val.v.int_val;
					}	
					if (right->tag == VAL_DOUBLE) {
						val.v.real_val += right->val.v.real_val;
					}
					else {
						val.v.real_val += right->val.v.int_val;
					}	
				}
				break;
			case MINUS_OP:
				if (val.tag == VAL_INT) {
					val.v.int_val = left->val.v.int_val - right->val.v.int_val;
				}
				if (val.tag == VAL_DOUBLE) {
					val.v.real_val = 0;
					if (left->tag == VAL_DOUBLE) {
						val.v.real_val += left->val.v.real_val;
					}
					else {
						val.v.real_val += left->val.v.int_val;
					}	
					if (right->tag == VAL_DOUBLE) {
						val.v.real_val -= right->val.v.real_val;
					}
					else {
						val.v.real_val -= right->val.v.int_val;
					}	
				}
				break;
			case MUL_OP:
				if (val.tag == VAL_INT) {
					val.v.int_val = left->val.v.int_val * right->val.v.int_val;
				}
				if (val.tag == VAL_DOUBLE) {
					val.v.real_val = 1;
					if (left->tag == VAL_DOUBLE) {
						val.v.real_val *= left->val.v.real_val;
					}
					else {
						val.v.real_val *= left->val.v.int_val;
					}	
					if (right->tag == VAL_DOUBLE) {
						val.v.real_val *= right->val.v.real_val;
					}
					else {
						val.v.real_val *= right->val.v.int_val;
					}	
				}
				break;
			default:
				break;
		}		
		/*if (left->val.tag == VAL_INT || right->val.tag == VAL_INT) {
			if (val.tag == VAL_DOUBLE) {
				val.tag = VAL_INT;
				val.v.int_val = val.v.real_val;
				fold_node->tag = VAL_INT;
				fold_node->type = build_base(update_bucket(NULL, INT_SPEC, NULL));
			}
		}*/
		fold_node->val = val;
		return fold_node;
	}
	EXPR_ND_PTR exp_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));

	exp_node->expr_type = BINARY;
	exp_node->op_type = op_type;
	if (flag == -1) {
		exp_node->type = ltype;
		exp_node->left = left;
		exp_node->right = right;
	}

	if (flag == 1) {
		EXPR_ND_PTR conv_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
		conv_node->expr_type = UNARY;
		conv_node->op_type = CONV_OP;
		conv_node->type = rtype;
		conv_node->right_type = left->type;
		conv_node->left = left;
		conv_node->right = NULL;
		exp_node->type = right->type;
		exp_node->left = conv_node;
		exp_node->right = right;
	}
		

	if (flag == 0) {
		EXPR_ND_PTR conv_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
		conv_node->expr_type = UNARY;
		conv_node->op_type = CONV_OP;
		conv_node->type = ltype;
		conv_node->right_type = right->type;
		conv_node->left = right;
		conv_node->right = NULL;
		exp_node->type = left->type;
		exp_node->left = left;
		exp_node->right = conv_node;
	}
	if (op_type == LESS_OP || op_type == GREATER_OP
		|| op_type == EQL_OP || op_type == NEQL_OP) {
		exp_node->right_type = exp_node->type;
		exp_node->type = build_base(update_bucket(NULL, INT_SPEC, NULL));
	}

	return exp_node;
}

EXPR_ND_PTR exp_mk_assign(EXPR_ND_PTR left, EXPR_ND_PTR right) {
	EXPR_ND_PTR exp_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
	TYPETAG ltag = ty_query(left->type);
	TYPETAG rtag = ty_query(right->type);
	exp_node->type = left->type;
	exp_node->expr_type = BINARY;
	exp_node->op_type = ASSIGN_OP;
	exp_node->left = left;
	if (left->op_type == DEREF_OP) {
		exp_node->left = left->left;
	}
	if (exp_node->left->val.tag == VAL_ST_ID) {
		int block = 0;
		if (st_lookup(exp_node->left->val.v.st_id_val, &block) == NULL) {
			exp_node->op_type = ERROR_OP;
			return exp_node;
		}
	}
	exp_node->right = right;
	TYPE right_type = exp_node->right->type;
	if (ty_query(right_type) == TYFUNC) {
		PARAMSTYLE pstyle;
		PARAM_LIST plist;
		right_type = ty_query_func(exp_node->right->type, &pstyle, &plist);
	}
	if (ty_query(exp_node->left->type) != ty_query(right_type)) {
		if (right->op_type == NO_OP && right->expr_type == LEAF 
			&& ty_query(exp_node->left->type) == TYSIGNEDINT
					&& ty_query(right_type) == TYDOUBLE) {
				exp_node->right->tag = VAL_INT;
				exp_node->right->type = exp_node->left->type;
				exp_node->right->val.tag = VAL_INT;
				exp_node->right->val.v.int_val = exp_node->right->val.v.real_val;
		}
		else {
		EXPR_ND_PTR conv_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
		conv_node->type = exp_node->left->type;
		//CHECK
		//conv_node->right_type = exp_node->right->type;
		conv_node->right_type = right_type;
		conv_node->expr_type = UNARY;
		conv_node->op_type = CONV_OP;
		conv_node->left = right;
		conv_node->right = NULL;
		exp_node->right = conv_node;
		}
	}

	return exp_node;
}

EXPR_ND_PTR exp_mk_real(VAL val) {
	EXPR_ND_PTR exp_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
	exp_node->type = build_base(update_bucket(NULL, DOUBLE_SPEC, NULL));
	exp_node->expr_type = LEAF;
	exp_node->op_type = NO_OP;
	exp_node->val = val;
	exp_node->tag = VAL_DOUBLE;
	exp_node->left = NULL;
	exp_node->right = NULL;
	return exp_node;
}

EXPR_ND_PTR exp_mk_int(VAL val) {
	EXPR_ND_PTR exp_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
	exp_node->type = build_base(update_bucket(NULL, INT_SPEC, NULL));
	exp_node->expr_type = LEAF;
	exp_node->op_type = NO_OP;
	exp_node->val = val;
	exp_node->tag = VAL_INT;
	exp_node->left = NULL;
	exp_node->right = NULL;
	return exp_node;
}

EXPR_ND_PTR exp_mk_var(VAL val) {
	ST_ID id = val.v.st_id_val;
	int block = 0;
	EXPR_ND_PTR exp_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
	ST_DR st_dr = st_lookup(id, &block);
	exp_node->expr_type = LEAF;
	exp_node->op_type = NO_OP;
	exp_node->val = val;
	exp_node->tag = VAL_ST_ID;
	exp_node->left = NULL;
	exp_node->right = NULL;
	if (st_dr != NULL) {
		exp_node->type = st_dr->u.decl.type;
	}
	else {
		exp_node->op_type = ERROR_OP;
		exp_node->type = build_base(update_bucket(NULL, INT_SPEC, NULL));
		return exp_node;;
	}

	EXPR_ND_PTR deref_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
	deref_node->type = st_dr->u.decl.type;
	deref_node->expr_type = UNARY;
	deref_node->op_type = DEREF_OP;
	deref_node->left = exp_node;
	deref_node->right = NULL;
	return deref_node;
}

EXPR_ND_PTR exp_mk_func_op(VAL val) {
	ST_ID id = val.v.st_id_val;
	EXPR_ND_PTR exp_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
	int block = 0;
	ST_DR st_dr = st_lookup(id, &block);
	if (st_dr == NULL) {
		return NULL;
	}
	exp_node->type = st_dr->u.decl.type;
	exp_node->expr_type = UNARY;
	exp_node->op_type = FUNC_OP;
	exp_node->val = val;
	exp_node->tag = VAL_ST_ID;
	exp_node->left = NULL;
	exp_node->right = NULL;
	return exp_node;
}

EXPR_ND_PTR mk_exp_unary_op(OP_TYPE op_type, EXPR_ND_PTR expr) {
	if(op_type == NEGATE_OP && expr->op_type == NO_OP && expr->val.tag != VAL_ST_ID) {
		VAL val = expr->val;
		if (val.tag == VAL_INT) {
			val.v.int_val= -expr->val.v.int_val;
		}
		else {
			val.v.real_val= -expr->val.v.real_val;
		}
		expr->val = val;
		return expr;
	}
	EXPR_ND_PTR exp_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
	exp_node->type = expr->type;
	exp_node->expr_type = UNARY;
	exp_node->op_type = op_type;
	exp_node->left = expr;
	exp_node->right = NULL;
	return exp_node;
}

EXPR_ND_PTR exp_mk_paren(EXPR_ND_PTR expr) {
	EXPR_ND_PTR exp_node = (EXPR_ND_PTR) malloc(sizeof(EXPR_ND));
	exp_node->type = expr->type;
	exp_node->expr_type = UNARY;
	exp_node->op_type = PAREN_OP;
	exp_node->left = expr;
	exp_node->right = NULL;
	return exp_node;
}

CONTROL_ND_PTR push_to_control_stack(CONTROL_ND_PTR top, CONTROL_TAG tag, char *exit_label) {
	CONTROL_ND_PTR new_top = (CONTROL_ND_PTR) malloc (sizeof(CONTROL_ND));
	new_top->next = top;
	new_top->tag = tag;
	new_top->default_label = NULL;
	new_top->pairs = NULL;
	new_top->exit_label = exit_label;
	return new_top;
}

CONTROL_ND_PTR pop_from_control_stack(CONTROL_ND_PTR top) {
	if (top == NULL) {
		return NULL;
	}
	CONTROL_ND_PTR new_top = top->next;
	top->next = NULL;
	free(top);
	return new_top;
}

char * peek_control_stack(CONTROL_ND_PTR top) {
	if (top == NULL) {
		return NULL;
	}
	return top->exit_label;
}

CONTROL_ND_PTR get_switch_closest_to_top(CONTROL_ND_PTR top) {
	while (top != NULL) {
		if (top->tag == IS_SWITCH) {
			return top;
		}
		top = top->next;
	}
	return NULL;
}
