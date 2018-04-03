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

