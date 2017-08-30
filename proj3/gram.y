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

TYPE mk_type_install(BUCKET_PTR bucket, DECL_ND_PTR head, STDR_TAG stdr_tag);
TYPE mk_type(BUCKET_PTR bucket, DECL_ND_PTR head);
BOOLEAN install_global_symbol(ST_ID id, TYPE type, STORAGE_CLASS sc, STDR_TAG stdr_tag);
ST_ID get_id_from_tree(DECL_ND_PTR head);
BOOLEAN check_dup_params(PARAM_LIST head, PARAM_LIST newParam);
TYPE param_install(ST_ID id, TYPE type);
PARAM_LIST get_param_from_func_node(DECL_ND_PTR head);
ST_ID handle_declarator_default(DECL_ND_PTR head);
ST_ID handle_declarator(BUCKET_PTR bucket, DECL_ND_PTR head);
void eval_expr(EXPR_ND_PTR expr);

BOOLEAN pop_on = TRUE;
char empty_label[10] = "";
//char *break_label = empty_label;
TYPETAG func_type_tag = TYVOID;
CONTROL_ND_PTR top = NULL;
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
	TYPE y_type;
	VAL y_consts;
	EXPR_ND_PTR y_expr;
	};

%type  <y_bucket> declaration_specifiers 
%type  <y_t_sp> type_specifier storage_class_specifier type_qualifier;
%type  <y_stid> identifier;
%type  <y_nd> direct_declarator declarator init_declarator_list init_declarator;
%type  <y_ptr> pointer;
%type  <y_param> parameter_type_list parameter_declaration parameter_list;
%type <y_expr> primary_expr expr_opt assignment_expr unary_expr multiplicative_expr 
%type <y_expr> additive_expr cast_expr relational_expr shift_expr equality_expr
%type <y_expr> expr postfix_expr 
%token <y_string> IDENTIFIER
%token <y_int> INT_CONSTANT
%type <y_int> assignment_operator unary_operator
%token <y_double> DOUBLE_CONSTANT
%type <y_string> if_action
%type  <y_expr> constant_expr;

%token <y_string> STRING_LITERAL 
%token SIZEOF 
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
	: identifier					{	VAL val;
										val.tag = VAL_ST_ID;
										val.v.st_id_val = $1;
										$$ = exp_mk_var(val);
									}
	| INT_CONSTANT					{	VAL val;
										val.tag = VAL_INT;
										val.v.int_val = $<y_int>1;
										$$ = exp_mk_int(val);
									}
	| DOUBLE_CONSTANT				{
										VAL val;
										val.tag = VAL_DOUBLE;
										val.v.real_val = $1;
										$$ = exp_mk_real(val);
									}
	| STRING_LITERAL
	| '(' expr ')'					{ $$ = exp_mk_paren($2);}
	;

postfix_expr
	: primary_expr
	| postfix_expr '[' expr ']'
	| postfix_expr '(' argument_expr_list_opt ')'		{
															//char *func_name = st_get_id_str($1->left->val.v.st_id_val);
															EXPR_ND_PTR name_exp = $1;
															if (name_exp->op_type == ERROR_OP) {
																//error("func defn not found");
																error("`%s' is undefined", st_get_id_str($1->val.v.st_id_val));
															}
															else {
															EXPR_ND_PTR expr = exp_mk_func_op($1->left->val);
															if (expr == NULL) {
																error("`%s' is undefined", st_get_id_str($1->left->val.v.st_id_val));
															}
															$$ = expr;
															}
														}
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
	| unary_operator cast_expr		{
										switch($1) {
											case 1:
												$$ = mk_exp_unary_op(NEGATE_OP, $2);
												break;
											default:
												break;
										}
									}
	| SIZEOF unary_expr
	| SIZEOF '(' type_name ')'
	;

unary_operator
	: '&' | '*' | '+' 
	| '-' 						{ $$ = 1; }
	| '~' | '!'
	;

cast_expr
	: unary_expr
	| '(' type_name ')' cast_expr
	;

multiplicative_expr
	: cast_expr
	| multiplicative_expr '*' cast_expr			{ $$ = exp_mk_binary_op(MUL_OP, $1, $3);	}
	| multiplicative_expr '/' cast_expr			{ $$ = exp_mk_binary_op(DIV_OP, $1, $3);	}
	| multiplicative_expr '%' cast_expr			{ $$ = exp_mk_binary_op(MOD_OP, $1, $3);	}
	;

additive_expr
	: multiplicative_expr
	| additive_expr '+' multiplicative_expr			{
														$$ = exp_mk_binary_op(PLUS_OP, $1, $3);
													}
	| additive_expr '-' multiplicative_expr			{ $$ = exp_mk_binary_op(MINUS_OP, $1, $3);	}
	;

shift_expr
	: additive_expr
	| shift_expr LEFT_OP additive_expr
	| shift_expr RIGHT_OP additive_expr
	;

relational_expr
	: shift_expr
	| relational_expr '<' shift_expr				{ $$ = exp_mk_binary_op(LESS_OP, $1, $3);	}
	| relational_expr '>' shift_expr				{ $$ = exp_mk_binary_op(GREATER_OP, $1, $3);}
	| relational_expr LE_OP shift_expr
	| relational_expr GE_OP shift_expr
	;

equality_expr
	: relational_expr
	| equality_expr EQ_OP relational_expr			{ $$ = exp_mk_binary_op(EQL_OP, $1, $3); }
	| equality_expr NE_OP relational_expr			{ $$ = exp_mk_binary_op(NEQL_OP, $1, $3); }
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
	| unary_expr assignment_operator assignment_expr		{ 
																switch ($2) {
																	case 1:
																		if ($1 == NULL) {
																			error("left arg is null");
																			break;
																		}
																		if ($3 == NULL) {
																			error("right arg is null");
																			break;
																		}
																		/*int block;
																		if (st_lookup($1->val.v.st_id_val, &block) == NULL) {
																			error("not found var");
																			break;
																		}*/
																		EXPR_ND_PTR expr = exp_mk_assign($1, $3); 
																		if (expr->op_type == ERROR_OP) {
																			error("`%s' is undefined", st_get_id_str(expr->left->val.v.st_id_val));
																			break;
																		}
																		$$ = expr;
																		break;	
																	case 2:
																		break;	
																	case 3:
																		break;	
																	default:
																		break;	
																}
															}
	;

assignment_operator
	: '=' 				{$$ = 1;}
	| MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN | ADD_ASSIGN | SUB_ASSIGN 
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
	: /* null derive */			{ $$ = NULL; pop_on = FALSE; }
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
	: init_declarator								
	| init_declarator_list ',' {$<y_bucket>$=$<y_bucket>0;} init_declarator		
	;

init_declarator
	: declarator									{
													DECL_ND_PTR head = $1;
													BUCKET_PTR bucket = $<y_bucket>0;
													//1. Make type from bucket and tree
													//2. Installing in symbol table
													//3. Calling assembly code generating backend routines
													TYPE type = mk_type_install(bucket, head, GDECL);
													// freeing the tree since code generating finished
													tr_free_tree($1);
													}
	| declarator 									{
													DECL_ND_PTR head = $1;
													BUCKET_PTR bucket = $<y_bucket>0;
													TYPE type = mk_type_install(bucket, head, GDECL);
													}
	'=' 											{
													DECL_ND_PTR head = $1;
													BUCKET_PTR bucket = $<y_bucket>0;
													TYPE type = mk_type(bucket, head);
														$<y_type>$ = type;
													tr_free_tree($1);
													}
	initializer					
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
															EXPR_ND_PTR expr_nd = $<y_expr>3;
															VAL val = expr_nd->val;
															//Inserting array node in tree
															$$ = tr_insert_array($1, val.v.int_val); 
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
	| CASE constant_expr 			{
										EXPR_ND_PTR expr = $2;
										if (expr->expr_type == LEAF && expr->op_type == NO_OP && expr->tag == VAL_INT) {
											CASE_PAIR_PTR case_pair = (CASE_PAIR_PTR) malloc (sizeof(CASE_PAIR));
											case_pair->val = expr->val.v.int_val;
											case_pair->label = new_symbol();
											b_label(case_pair->label);
											CONTROL_ND_PTR switch_nd = get_switch_closest_to_top(top);
											if (switch_nd == NULL) {
												error("case label not inside switch");
											}
											else {
												//Check if case value already exist in value-label pairs
												CASE_PAIR_PTR existing_pair = switch_nd->pairs;
												while (existing_pair != NULL) {
													if (existing_pair->val == expr->val.v.int_val) {
														error("duplicate value in case label: %d", expr->val.v.int_val);
													}
													existing_pair = existing_pair->next;
												}
												case_pair->next = switch_nd->pairs;
												switch_nd->pairs = case_pair;
											}
										}
									}
	':' statement
	| DEFAULT 						{
										char *default_label = new_symbol();
										b_label(default_label);
										CONTROL_ND_PTR switch_nd = get_switch_closest_to_top(top);
										if (switch_nd->default_label == NULL) {
											switch_nd->default_label = default_label;
										}
										else {
											error("duplicate default label inside switch");
										}
									}
	':' statement
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
	: expr_opt ';'								{
													EXPR_ND_PTR expr = $1;
													eval_expr(expr);
													if (pop_on) {
														b_pop();
													}
													pop_on = TRUE;
												}
	;

selection_statement
	: IF '(' expr ')' if_action statement						{
																//	eval_expr($6);
																	b_label($5);
																}
	| IF '(' expr ')' if_action statement ELSE 					{
																	char *label = new_symbol ();
																	//eval_expr($6);
																	b_jump(label);
																	$<y_string>$ = label;
																	b_label($5);
																}
		statement												{
																//	eval_expr($9);
																	b_label($<y_string>8);
																}
	| SWITCH '(' expr ')' 				{
											EXPR_ND_PTR expr = $3;
											if (ty_query(expr->type) == TYSIGNEDINT
												|| ty_query(expr->type) == TYSIGNEDCHAR) {
												eval_expr(expr);
												if (ty_query(expr->type) == TYSIGNEDCHAR) {
													b_convert(TYSIGNEDCHAR, TYSIGNEDINT);
												}
												char *start_label = new_symbol();
												$<y_string>$ = start_label;
											}
											else {
												error("switch expression not of integral type");
											}
										}
										{
											char *exit_label = new_symbol();
											$<y_string>$ = exit_label;
											top = push_to_control_stack(top, IS_SWITCH, exit_label);
											top->default_label = NULL;
											top->pairs = NULL;
											b_jump($<y_string>5);
										}
	statement							{
											b_jump(top->exit_label);
											b_label($<y_string>5);
											CASE_PAIR_PTR case_head = top->pairs;
											while (case_head != NULL) {
												b_dispatch(B_EQ, TYSIGNEDINT, case_head->val, 
													case_head->label, TRUE);
												case_head = case_head->next;
											}
											b_pop();
											if (top->default_label != NULL) {
												b_jump(top->default_label);
											}
											b_label(top->exit_label);
											top =  pop_from_control_stack(top);
										}
	;

if_action
	: /* empty */								{
													eval_expr($<y_expr>-1);
													char *label = new_symbol ();
													b_cond_jump(TYSIGNEDINT, B_ZERO, label);
													$$ = label;
												}

iteration_statement
	: WHILE '(' expr ')' 								{
															char *label = new_symbol();
															b_label(label);
															eval_expr($3);
															$<y_string>$ = label;
														}
														{
															char *label = new_symbol();
															b_cond_jump(TYSIGNEDINT, B_ZERO, label);
															$<y_string>$ = label;
															top = push_to_control_stack(top, NOT_SWITCH, label);
															//printf("push %s\n", top->exit_label);
															//break_label = label;
														}
		statement										{
															//eval_expr($7);
															b_jump($<y_string>5);
															b_label($<y_string>6);
															//break_label = empty_label;
															//printf("pop %s\n", top->exit_label);
															top = pop_from_control_stack(top);
														}
	| DO statement WHILE '(' expr ')' ';'
	| FOR '(' expr_opt ';' expr_opt ';' expr_opt ')' 	{
															EXPR_ND_PTR expr1 = $3;
															char *label = new_symbol();
															if (expr1 != NULL) {
																eval_expr(expr1);
																TYPETAG tag = ty_query(expr1->type);
																if (tag != TYVOID) {
																	b_pop();
																}
															}
															else {
																pop_on = TRUE;
															}
															b_label(label);
															$<y_string>$ = label;
														}
														{
															EXPR_ND_PTR expr2 = $5;
															char *exit_label = new_symbol();
															if (expr2 != NULL) {
																eval_expr(expr2);
															b_cond_jump(TYSIGNEDINT, B_ZERO, exit_label);
															}
															$<y_string>$ = exit_label;
															//break_label = exit_label;
															top = push_to_control_stack(top, NOT_SWITCH, exit_label);
															//printf("exit label %s\n", top->exit_label);
														}
		statement										{
															char *label = new_symbol();
															b_label(label);
															EXPR_ND_PTR expr3 = $7;
															if (expr3 != NULL) {
																eval_expr(expr3);
																TYPETAG tag = ty_query(expr3->type);
																if (tag != TYVOID) {
																	b_pop();
																}
															}
															if (strlen($<y_string>9) > 0) {
																b_jump($<y_string>9);
															}
															if (strlen($<y_string>10) > 0) {
																b_label($<y_string>10);
																//break_label = $<y_string>10;
															}
															/*else {
																break_label = empty_label;
															}*/
															top = pop_from_control_stack(top);
														}
	;

jump_statement
	: GOTO identifier ';'
	| CONTINUE ';'
	| BREAK ';'						{
										char * break_label = peek_control_stack(top);
										if (break_label != NULL && strlen(break_label) > 0) {
													//printf("sdddddddddddddddddddddddddddddd %s\n", break_label);
											b_jump(break_label);
											//break_label = empty_label;
										}
										else {
											error("break not inside switch or loop");
										}
									}
	| RETURN expr_opt ';'			{
										EXPR_ND_PTR expr = $2;
										eval_expr(expr);
										if (func_type_tag != ty_query(expr->type)) {
											b_convert(ty_query(expr->type), func_type_tag);
											b_encode_return(func_type_tag);
											func_type_tag =  TYVOID;
										}
										else {
											b_encode_return(ty_query(expr->type));
										}
									}
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
	: declarator 						{	
											ST_ID st_id = handle_declarator_default($1);
											$<y_stid>$ = st_id;
										}
										{
											char *label = new_symbol();
											$<y_string>$ = label;
										}
	compound_statement					{	
											b_label($<y_string>3);
											b_func_epilogue(st_get_id_str($<y_stid>2));
											st_exit_block();
										}
	| declaration_specifiers declarator {
											DECL_ND_PTR head = $2;
											BUCKET_PTR bucket = $1;
											ST_ID st_id = handle_declarator(bucket, head);
											$<y_stid>$ = st_id;
										}
										{
											char *label = new_symbol();
											$<y_string>$ = label;
										}
	compound_statement					{
											b_label($<y_string>4);
											b_func_epilogue(st_get_id_str($<y_stid>3));
											st_exit_block();
										}
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

void eval_expr(EXPR_ND_PTR expr) {
	if (expr == NULL) {
		return;
	}
	eval_expr(expr->left);
	eval_expr(expr->right);
	switch (expr->expr_type) {
		case LEAF:
			switch (expr->tag) {
				case VAL_INT:
					b_push_const_int(expr->val.v.int_val);
					break;
				case VAL_DOUBLE:
					b_push_const_double(expr->val.v.real_val);
					break;
				case VAL_ST_ID:
					b_push_ext_addr(st_get_id_str(expr->val.v.st_id_val));
					break;
				default:
					break;
			}
			break;
		case UNARY:
			switch (expr->op_type) {
				case DEREF_OP:
					b_deref(ty_query(expr->type));
					break;
				case CONV_OP:
					if (ty_query(expr->right_type) == TYSIGNEDINT
						&& ty_query(expr->type) == TYSIGNEDCHAR) {
						//Do nothing
					}
					else {
						b_convert(ty_query(expr->right_type), ty_query(expr->type));
					}
					break;
				case NEGATE_OP:
					b_negate(ty_query(expr->type));
					break;
				case FUNC_OP:
					b_alloc_arglist(0);
					if (ty_query(expr->type) != TYFUNC) {
						error("expression not of function type");
						break;
					}
					PARAM_LIST plist;
					PARAMSTYLE pstyle;
					TYPE type = ty_query_func(expr->type, &pstyle, &plist);
					TYPETAG tag = ty_query(type);
					b_funcall_by_name(st_get_id_str(expr->val.v.st_id_val), tag);
					if (tag == TYVOID) {
						pop_on = FALSE;
					}
					break;
				default:
					break;
			}
			break;
		case BINARY:
			switch (expr->op_type) {
				case ASSIGN_OP:
					if (ty_query(expr->type) == TYFUNC) {
						error("left side of assignment is not an l-value");
						break;
					}
					b_assign(ty_query(expr->type));
					break;
				case PLUS_OP:
					b_arith_rel_op(B_ADD, ty_query(expr->type));
					break;
				case MINUS_OP:
					b_arith_rel_op(B_SUB, ty_query(expr->type));
					break;
				case DIV_OP:
					b_arith_rel_op(B_DIV, ty_query(expr->type));
					break;
				case MUL_OP:
					b_arith_rel_op(B_MULT, ty_query(expr->type));
					break;
				case LESS_OP:
					b_arith_rel_op(B_LT, ty_query(expr->right_type));
					break;
				case GREATER_OP:
					b_arith_rel_op(B_GT, ty_query(expr->right_type));
					break;
				case MOD_OP:
					b_arith_rel_op(B_MOD, ty_query(expr->type));
					break;
				case EQL_OP:
					b_arith_rel_op(B_EQ, ty_query(expr->type));
					break;
				case NEQL_OP:
					b_arith_rel_op(B_NE, ty_query(expr->type));
					break;
				default:
					break;
			}
			break;
		default:
			break;
	}
}

ST_ID handle_declarator_default(DECL_ND_PTR head) {
	BUCKET_PTR bucket = update_bucket(NULL, INT_SPEC, NULL);
	return handle_declarator(bucket, head);
}

ST_ID handle_declarator(BUCKET_PTR bucket, DECL_ND_PTR head) {
											TYPE type = mk_type(bucket, head);
											PARAMSTYLE pstyle;
											PARAM_LIST plist;
											func_type_tag = ty_query(ty_query_func(type, &pstyle, &plist));
											if (ty_query(type) != TYFUNC) {
												error("not func\n");
											}
											int blk = 0;
											ST_ID st_id = get_id_from_tree(head);
											ST_DR st_dr = st_lookup(st_id, &blk);
											if (st_dr != NULL) {
												if (st_dr->tag == FDECL) {
													error("same func def--fail\n");
												}
												if (st_dr->tag == GDECL) {
													if (st_dr->u.decl.type != type) {
														error("duplicate or incompatible function declaration `%s'", st_get_id_str(st_id));
													}
													st_dr->tag = FDECL;
													st_replace(st_id, st_dr);
												}
												else {
													error("semantic error -- fail\n");
												}
											}
											else {
												TYPE type = mk_type_install(bucket, head, FDECL);
											}
											st_enter_block();
											b_func_prologue(st_get_id_str(st_id));
											PARAM_LIST phead = get_param_from_func_node(head);
											while(phead != NULL) {
												int offset = b_store_formal_param(ty_query(phead->type));
												param_install(phead->id, phead->type); 
												ST_DR st_dr = st_lookup(phead->id, &blk);
												st_dr->u.decl.binding = offset;
												st_replace(phead->id, st_dr);
												phead = phead->next;
											}
	return st_id;
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

PARAM_LIST get_param_from_func_node(DECL_ND_PTR head) {
	while (head != NULL) {
		if (head->type == FUNC_DECL) {
			return head->params;
			break;
		}
		head = head->next;
	}
	return NULL;
}

TYPE param_install(ST_ID id, TYPE type) {

	STORAGE_CLASS sc = NO_SC;
	
	TYPE finalType = type;
	TYPETAG tag = ty_query(type);
	TYPETAG finalTag = tag;
	BOOLEAN success = install_global_symbol(id, finalType, sc, PDECL);
	if (success == FALSE) {
		return NULL;
	}
	return finalType;
}
//1. Make type
//2. Install in symbol table
//3. generate code using backend
TYPE mk_type_install(BUCKET_PTR bucket, DECL_ND_PTR head, STDR_TAG stdr_tag) {
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
	BOOLEAN success = install_global_symbol(id, finalType, sc, stdr_tag);
	if (success == FALSE) {
		return NULL;
	}

	if (finalTag == TYFUNC) {
		return finalType;
	}
	spit_code(id, tag, total_dim);
	return finalType;
}

/* Install on symbol table */
BOOLEAN install_global_symbol(ST_ID id, TYPE type, STORAGE_CLASS sc, STDR_TAG  stdr_tag) {
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
