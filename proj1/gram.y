/*
 *
 * yacc/bison input for simplified C++ parser
 *
 */

%{

#include "defs.h"
#include "types.h"
#include "symtab.h"
#include "bucket.h"
#include "message.h"
#include "tree.h"
#include "encode.h"
#include BACKEND_HEADER_FILE

    int yylex();
    int yyerror(char *s);

TYPE mk_type_install(BUCKET_PTR bucket, DECL_ND_PTR head);
TYPE mk_type(BUCKET_PTR bucket, DECL_ND_PTR head);
BOOLEAN install_global_symbol(ST_ID id, TYPE type, STORAGE_CLASS sc);
ST_ID get_id_from_tree(DECL_ND_PTR head);
BOOLEAN check_dup_params(PARAM_LIST head, PARAM_LIST newParam);
%}

%union {
	int	y_int;
	double	y_double;
	char *	y_string;
	BUCKET_PTR y_bucket;
	ST_ID * y_stid;
	DECL_ND_PTR y_nd;
	PARAM_LIST y_param;
	BOOLEAN y_ptr;
	TYPE_SPECIFIER y_t_sp;
	};

%type  <y_bucket> declaration_specifiers 
%type  <y_t_sp> type_specifier storage_class_specifier type_qualifier;
%type  <y_stid> identifier;
%type  <y_nd> direct_declarator declarator init_declarator_list init_declarator;
%type  <y_int> constant_expr;
%type  <y_ptr> pointer;
%type  <y_param> parameter_type_list parameter_declaration parameter_list;

%token <y_string> IDENTIFIER DOUBLE_CONSTANT STRING_LITERAL SIZEOF
%token <y_int> INT_CONSTANT
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token BAD

%start translation_unit
%%

 /*******************************
  * Expressions                 *
  *******************************/

primary_expr
	: identifier
	| INT_CONSTANT
	| DOUBLE_CONSTANT
	| STRING_LITERAL
	| '(' expr ')'
	;

postfix_expr
	: primary_expr
	| postfix_expr '[' expr ']'
	| postfix_expr '(' argument_expr_list_opt ')'
	| postfix_expr '.' identifier
	| postfix_expr PTR_OP identifier
	| postfix_expr INC_OP
	| postfix_expr DEC_OP
	;

argument_expr_list_opt
	: /* null derive */
	| argument_expr_list
	;

argument_expr_list
	: assignment_expr
	| argument_expr_list ',' assignment_expr
	;

unary_expr
	: postfix_expr
	| INC_OP unary_expr
	| DEC_OP unary_expr
	| unary_operator cast_expr
	| SIZEOF unary_expr
	| SIZEOF '(' type_name ')'
	;

unary_operator
	: '&' | '*' | '+' | '-' | '~' | '!'
	;

cast_expr
	: unary_expr
	| '(' type_name ')' cast_expr
	;

multiplicative_expr
	: cast_expr
	| multiplicative_expr '*' cast_expr
	| multiplicative_expr '/' cast_expr
	| multiplicative_expr '%' cast_expr
	;

additive_expr
	: multiplicative_expr
	| additive_expr '+' multiplicative_expr
	| additive_expr '-' multiplicative_expr
	;

shift_expr
	: additive_expr
	| shift_expr LEFT_OP additive_expr
	| shift_expr RIGHT_OP additive_expr
	;

relational_expr
	: shift_expr
	| relational_expr '<' shift_expr
	| relational_expr '>' shift_expr
	| relational_expr LE_OP shift_expr
	| relational_expr GE_OP shift_expr
	;

equality_expr
	: relational_expr
	| equality_expr EQ_OP relational_expr
	| equality_expr NE_OP relational_expr
	;

and_expr
	: equality_expr
	| and_expr '&' equality_expr
	;

exclusive_or_expr
	: and_expr
	| exclusive_or_expr '^' and_expr
	;

inclusive_or_expr
	: exclusive_or_expr
	| inclusive_or_expr '|' exclusive_or_expr
	;

logical_and_expr
	: inclusive_or_expr
	| logical_and_expr AND_OP inclusive_or_expr
	;

logical_or_expr
	: logical_and_expr
	| logical_or_expr OR_OP logical_and_expr
	;

conditional_expr
	: logical_or_expr
	| logical_or_expr '?' expr ':' conditional_expr
	;

assignment_expr
	: conditional_expr
	| unary_expr assignment_operator assignment_expr
	;

assignment_operator
	: '=' | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN | ADD_ASSIGN | SUB_ASSIGN
	| LEFT_ASSIGN | RIGHT_ASSIGN | AND_ASSIGN | XOR_ASSIGN | OR_ASSIGN
	;

expr
	: assignment_expr
	| expr ',' assignment_expr
	;

constant_expr
	: conditional_expr
	;

expr_opt
	: /* null derive */
	| expr
	;

 /*******************************
  * Declarations                *
  *******************************/

declaration
	: declaration_specifiers ';'								{
																	//Error because no decl ex. signed int;
																	//Should be signed int a;
																	error("no declarator in declaration"); 
																}
	| declaration_specifiers init_declarator_list ';' 			 
	;

declaration_specifiers
	: storage_class_specifier									{ $$ = update_bucket(NULL, $1, NULL); }
	| storage_class_specifier declaration_specifiers			{ $$ = update_bucket($2, $1, NULL); }
	| type_specifier											{ $$ = update_bucket(NULL, $1, NULL); }
	| type_specifier declaration_specifiers						{ $$ = update_bucket($2, $1, NULL); }
	| type_qualifier											{ $$ = update_bucket(NULL, $1, NULL); }
	| type_qualifier declaration_specifiers						{ $$ = update_bucket($2, $1, NULL); }
	;

init_declarator_list
	: init_declarator								{
													DECL_ND_PTR head = $1;
													BUCKET_PTR bucket = $<y_bucket>0;
													//1. Make type from bucket and tree
													//2. Installing in symbol table
													//3. Calling assembly code generating backend routines
													TYPE type = mk_type_install(bucket, head);
													// freeing the tree since code generating finished
													tr_free_tree($1);
													}
	| init_declarator_list ',' init_declarator		{
													DECL_ND_PTR head = $3;
													BUCKET_PTR bucket = $<y_bucket>0;
													//1. Make type from bucket and tree
													//2. Installing in symbol table
													//3. Calling assembly code generating backend routines
													TYPE type = mk_type_install(bucket, head);
													// freeing the tree since code generating finished
													tr_free_tree($3);
													}
	;

init_declarator
	: declarator
	| declarator '=' initializer
	;

storage_class_specifier
	: TYPEDEF 										{ $$ = TYPEDEF_SPEC; }
	| EXTERN										{ $$ = EXTERN_SPEC; } 
	| STATIC 										{ $$ = STATIC_SPEC; }
	| AUTO											{ $$ = AUTO_SPEC; } 
	| REGISTER										{ $$ = REGISTER_SPEC; }
	;

type_specifier
	: VOID 											{ $$ = VOID_SPEC; }
	| CHAR 											{ $$ = CHAR_SPEC; }
	| SHORT											{ $$ = SHORT_SPEC; }
	| INT											{ $$ = INT_SPEC; }
	| LONG 											{ $$ = LONG_SPEC; }
	| FLOAT											{ $$ = FLOAT_SPEC; }
	| DOUBLE										{ $$ = DOUBLE_SPEC; }
	| SIGNED										{ $$ = SIGNED_SPEC; }
	| UNSIGNED										{ $$ = UNSIGNED_SPEC; }
	| struct_or_union_specifier
	| enum_specifier
	| TYPE_NAME
	;

struct_or_union_specifier
	: struct_or_union '{' struct_declaration_list '}'
	| struct_or_union identifier '{' struct_declaration_list '}'
	| struct_or_union identifier
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list_opt
	| type_qualifier specifier_qualifier_list_opt
	;

specifier_qualifier_list_opt
	: /* null derive */
	| specifier_qualifier_list
        ;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: declarator
	| ':' constant_expr
	| declarator ':' constant_expr
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM identifier '{' enumerator_list '}'
	| ENUM identifier
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator
	: identifier
	| identifier '=' constant_expr
	;

type_qualifier
	: CONST 							{$$ = CONST_SPEC; }
	| VOLATILE							{$$ = VOLATILE_SPEC; }
	;

declarator
	: direct_declarator
	| pointer declarator			{ BOOLEAN ref = $1; 
									if (ref) {
										//Adding reference node in tree
										$$ = tr_insert_ref($2); 
									}
									  else  {
										//Adding pointer node to top of tree
										$$ = tr_insert_ptr($2); 
										}
									}
	;

direct_declarator
	: identifier					{
											//inserting id node in tree
											$$ = tr_insert_id($1); 
									}
	| '(' declarator ')'			{
											//Ignoring parenthesis
											$$ = $2; 
									}
	| direct_declarator '[' ']'
	| direct_declarator '[' constant_expr ']' 		{
															//Inserting array node in tree
															$$ = tr_insert_array($1, $3); 
													}
	| direct_declarator '(' parameter_type_list ')' {
														//Inserting function node in tree with params
														$$ = tr_insert_func_params($1, $3);
													}
	| direct_declarator '(' ')'						{
														//Inserting function node -- need to DELETE this
														$$ = tr_insert_func($1);
													}
	;

pointer
	: '*' specifier_qualifier_list_opt				{$$ = FALSE;}
        | '&'										{$$ = TRUE;}
	;

parameter_type_list
	: parameter_list
	| parameter_list ',' ELIPSIS
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration		{
														//Append parameter in $3 to parameter list in $1
														PARAM_LIST p1 = $1;
														PARAM_LIST p2 = $3;
														if (p2 == NULL) {
															//If parameter node is null return original list in $1
															$$ = p1;
														}
														else {
														//Check if parameter already exists in parameter list before adding to list
														BOOLEAN is_dup = check_dup_params(p1, p2);
														if (!is_dup) {
															//Loop to reach end of list
															while(p1->next != NULL) {
																p1 = p1->next;
															}
															//Add parameter to end of list
															p2->prev = p1;
															p1->next = p2;
															$$ = $1;
														}
														else {
															//Error on duplicate parameters in list
															error("duplicate parameter declaration for `%s'", st_get_id_str(p2->id));
															//Return original list
															$$ = p1;
														}
														}
													}
	;

parameter_declaration
	: declaration_specifiers declarator				{
													DECL_ND_PTR head = $2;
													BUCKET_PTR bucket = $1;
													//TYPE from bucket and tree
													TYPE type = mk_type(bucket, head);
													//Get variable from tree
													ST_ID id = get_id_from_tree(head);
													PARAM_LIST param = plist_alloc();
													param->id = id;
													param->type = type;
													//Check if * or &
													BOOLEAN ref = is_ptr(head);
													param->is_ref = !ref;
													//param->err = FALSE;
													param->next = NULL;
													param->prev = NULL;
													tr_free_tree(head);
													$$ =  param;
													}
	| declaration_specifiers						{error("no id in parameter list"); $$ = NULL;}
	| declaration_specifiers abstract_declarator
	;

type_name
	: specifier_qualifier_list
	| specifier_qualifier_list abstract_declarator
	;

abstract_declarator
	: pointer
	| direct_abstract_declarator
	| pointer abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' constant_expr ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' constant_expr ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

initializer
	: assignment_expr
	| '{' initializer_list comma_opt '}'
	;

comma_opt
	: /* Null derive */
	| ','
	;

initializer_list
	: initializer
	| initializer_list ',' initializer
	;

 /*******************************
  * Statements                  *
  *******************************/

statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

labeled_statement
	: identifier ':' statement
	| CASE constant_expr ':' statement
	| DEFAULT ':' statement
	;

compound_statement
	: '{' '}'
	| '{' statement_list '}'
	| '{' declaration_list '}'
	| '{' declaration_list statement_list '}'
	;

declaration_list
	: declaration
	| declaration_list declaration
	;

statement_list
	: statement
	| statement_list statement
	;

expression_statement
	: expr_opt ';'
	;

selection_statement
	: IF '(' expr ')' statement
	| IF '(' expr ')' statement ELSE statement
	| SWITCH '(' expr ')' statement
	;

iteration_statement
	: WHILE '(' expr ')' statement
	| DO statement WHILE '(' expr ')' ';'
	| FOR '(' expr_opt ';' expr_opt ';' expr_opt ')' statement
	;

jump_statement
	: GOTO identifier ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN expr_opt ';'
	;

 /*******************************
  * Top level                   *
  *******************************/

translation_unit
	: external_declaration
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: declarator compound_statement
	| declaration_specifiers declarator compound_statement
	;

 /*******************************
  * Identifiers                 *
  *******************************/

identifier
	: IDENTIFIER		{$$ = st_enter_id($1);}
	;
%%

extern int column;

int yyerror(char *s)
{
	error("%s (column %d)",s,column);
        return 0;  /* never reached */
}

/* function checks if pointer is * or &. This is for parameter is reference true for false*/
BOOLEAN is_ptr(DECL_ND_PTR head) {
	while (head != NULL) {
		//Loop on tree until reference node found
		if (head->type == REF_DECL) {
			// If reference node found then pointer type is &
			return FALSE;
		}
		head =  head->next;
	}
	//pointer type is *
	return TRUE;
}

TYPE mk_type(BUCKET_PTR bucket, DECL_ND_PTR head) {
	//Build basic type from bucket
	TYPE type = build_base(bucket);
	//Loop through tree to make type
	while(head != NULL) {
		if (head->type == ARR_DECL) {
			type = ty_build_array(type, DIM_PRESENT, head->arr_size);
		}
		if (head->type == PTR_DECL) {
			type = ty_build_ptr(type, NULL);
		}
		if (head->type == FUNC_DECL) {
			if (head->params == NULL) {
				//Delete this --- 
				type = ty_build_func(type, NULL, NULL);
			}
			else {
				//params were set in tree when making fuction node
				type = ty_build_func(type, PROTOTYPE, head->params);
			}
		}
		if (head->type == ID_DECL) {
			break;
		}
		head = head->next;
	}
	return type;
}

/* Function return variable name from tree */
ST_ID get_id_from_tree(DECL_ND_PTR head) {
	ST_ID id = NULL;
	while(head != NULL) {
		if (head->type == ID_DECL) {
			id = head->id;
		}
		head = head->next;
	}
	return id;
}

//1. Make type
//2. Install in symbol table
//3. generate code using backend
TYPE mk_type_install(BUCKET_PTR bucket, DECL_ND_PTR head) {
	ST_ID id = get_id_from_tree(head);
	TYPE type = mk_type(bucket, head);

	STORAGE_CLASS sc = get_class(bucket);
	
	TYPE finalType = type;
	TYPETAG tag = ty_query(type);
	TYPETAG finalTag = tag;
	DIMFLAG dimflag = NO_DIM;
	unsigned int total_dim = 1;
	//Array size of int a[6] will be 24 and a[2][2] will be 16
	while (tag == TYARRAY) {
		unsigned int dim = 1;
		type = ty_query_array(type, &dimflag, &dim);
		total_dim *= dim;
		if (dim == 0) {
			total_dim = 0;
			break;
		}
		tag = ty_query(type);
		//Error on array of function
		if (tag == TYFUNC) {
			error("cannot have array of functions");
			return finalType;
		}
	}
	if (tag == TYFUNC) {
		PARAMSTYLE paramstyle;
		PARAM_LIST params;
		type = ty_query_func(type, &paramstyle, &params);
		/*while (params != NULL) {
			PARAM_LIST tmp = params->next;
			free(params);
			params = tmp;
		}*/
		tag = ty_query(type);
		if (tag == TYFUNC) {
			error("cannot have function returning function");
			return finalType;
		}
		if (tag == TYARRAY) {
			error("cannot have function returning array");
			return finalType;
		}
	}
	if (total_dim < 1) {
		error("illegal array dimension\n");
		return NULL;
	}
	BOOLEAN success = install_global_symbol(id, finalType, sc);
	if (success == FALSE) {
		return NULL;
	}

	if (finalTag == TYFUNC) {
		return finalType;
	}
	if (compiler_errors == 0) {
		spit_code(id, tag, total_dim);
	}
	return finalType;
}

/* Install on symbol table */
BOOLEAN install_global_symbol(ST_ID id, TYPE type, STORAGE_CLASS sc) {
	ST_DR st_dr = stdr_alloc();
	st_dr->tag = GDECL;
	st_dr->u.decl.type = type;
	st_dr->u.decl.sc = sc;
	BOOLEAN success = st_install(id, st_dr);
	if (success == FALSE) {
		error("duplicate declaration for %s", st_get_id_str(id));
		error("duplicate definition of `%s'", st_get_id_str(id));
	}
	return success;
}

/* check if a parameter exists in PARAM_LIST */
/* int a(int b, int c, int b) is illegal */
BOOLEAN check_dup_params(PARAM_LIST head, PARAM_LIST newParam) {
	if (newParam == NULL || newParam->id == NULL) {
		return FALSE;
	}
	if (st_get_id_str(newParam->id) == NULL) {
		return FALSE;
	}
	if (strlen(st_get_id_str(newParam->id)) == 0) {
		return FALSE;
	}
	
	while (head != NULL) {
		if (strcmp(st_get_id_str(newParam->id), st_get_id_str(head->id)) == 0) {
			return TRUE;
		}
		head = head->next;
	}
	return FALSE;
}
