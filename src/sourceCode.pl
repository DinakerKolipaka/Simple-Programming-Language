ii/*********************************************************************/
Lexer Code
/*********************************************************************/







/*********************************************************************/
Parser Code
/*********************************************************************/







/*********************************************************************/
Interpreter Code
/*********************************************************************/
/* Currently all operations, assignment, if-then-else statements are working */

/* high-level predicate to call the interpreter program. */
eval_Program(T) :-	add_to_env([[x,10], [b, 'false']], [y,20], Env), 
    			eval_exp_Program(T, Env, _).
    

                             
/* E1 = E0 & {I, V} */
add_to_env(E, L, Env) :-	length(E, N),
    			 	N = 0,
				Env = [L].
    							

add_to_env(E, L, Env) :- 	length(E, N),
    			 	N > 0,
				append(E, [L], Env).


/* evaluation function for operators */
eval_exp_Iden('+'(T, E), R, Env, Env_New) :- 	eval_term_Iden(T, R1, Env, Env_N),
    						!,
						eval_exp_Iden(E, R2, Env_N, Env_New),
    						R is R1 + R2, !.

eval_exp_Iden('-'(T, E), R, Env, Env_New) :- 	eval_term_Iden(T, R1, Env, Env_N),
    						!,
						eval_exp_Iden(E, R2, Env_N, Env_New),
    						R is R1 - R2, !.

eval_exp_Iden('*'(T, E), R, Env, Env_New) :- 	eval_term_Iden(T, R1, Env, Env_N),
    						!,
						eval_exp_Iden(E, R2, Env_N, Env_New),
    						R is R1 * R2, !.


eval_exp_Iden('/'(T, E), R, Env, Env_New) :-	eval_term_Iden(T, R1, Env, Env_N),
    						!,
						eval_exp_Iden(E, R2, Env_N, Env_New),
    						R is R1 / R2, !.

/* eval func for mod */
eval_exp_Iden(em(T, E), R, Env, Env_New)  :- 	eval_term_Iden(T, R1, Env, Env_N),			
    						!,
	    					eval_exp_Iden(E, R2, Env_N, Env_New),
    						R is mod(R1,R2), !.

eval_exp_Iden('='(T, E), R, Env, Env_New) :- 	eval_term_Iden(T, R, Env, Env_N),
	    					!,
    						eval_exp_Iden(E, R2, Env_N, Env_NN),
    						add_to_env(Env_NN, [R, R2], Env_New)
    						!.

/* eval func for terminal */
eval_exp_Iden(et(T), R, Env, Env_New) :-	eval_term_Iden(T, R, Env, Env_new).
		



eval_term_Iden(t(T), T, _) :-			term_Iden(T).

/* when terminl is identifier */
eval_term_Iden(t(T), R, Env, Env) :-		\+ term_Iden(T),
    						look_up(T, Value, Env),
    						R = Value.

term_Iden(T) :- 				atom_codes(T, F),
    						F > 47, F < 58.

/* eval for only if-then condition*/
eval_exp_If('if'(If, Then), Env, Env_New) :- 	eval_exp_Iden(If, Bool, Env, Env_N),
    						!,
    						is_true(Bool),
	    					eval_exp_Statement(Then, Env_N, Env_New).


eval_exp_If_Then('if'(If, Then, _), Env, Env_New) :- 	eval_exp_Iden(If, Bool, Env, Env_N),
    							!,
    							is_true(Bool), 
	    						eval_exp_Statement(Then, Env_N, Env_New).

eval_exp_If_Else('if'(If, _, Else), Env, Env_New) :- 	eval_exp_Iden(If, Bool, Env, Env_N),
    							!,
    							\+ is_true(Bool),
	    						eval_exp_Statement(Else, Env_N, Env_New).

eval_exp_Statement('statement'(T), Env, Env_New) :- 	eval_exp_Iden(T, R, Env, Env_New),
    							!,
    							write(R),
    							nl, !.

eval_exp_Statement('statement'(T), Env, Env_New) :- 	eval_exp_If(T, Env, Env_New).
eval_exp_Statement('statement'(T), Env, Env_New) :- 	eval_exp_If_Then(T, Env, Env_New).
eval_exp_Statement('statement'(T), Env, Env_New) :- 	eval_exp_If_Else(T, Env, Env_New).

eval_exp_Statement('then'(T), Env, Env_New) :- 		eval_exp_Statement(T, Env, Env_New).

eval_exp_Statement('else'(T), Env, Env_New) :- 		eval_exp_Statement(T, Env, Env_New).

    											
eval_exp_Statement_List('statement_List'(T, E), Env, Env_New) :- 	eval_exp_Statement(T, Env, Env_N),
    									eval_exp_Statement_List(E, Env_N, Env_New).


eval_exp_Statement_List('statement_List'(T), Env, Env_New) :- 		eval_exp_Statement(T, Env, Env_New).
    														

eval_exp_Program('program'(T), Env, Env_New) :- 	eval_exp_Statement_List(T, Env, Env_New).


/* look-up for variables in env */
look_up(Id, Value, [H|T]) :- 	look_Id(Id, Value, H),
    				look_up(Id, Value, T).

look_up(_, _, []).

look_up(Id, Value, H) :- 	look_Id(Id, Value, H).
    								

look_Id(Id, Value, [Id, Value]).
look_Id(_, _, _).

is_true(Bool) :- Bool = 'true'.

