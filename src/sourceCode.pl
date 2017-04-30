/*********************************************************************/
Lexer Code
/*********************************************************************/

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).


eos([], []).


statement_list_L([])          	--> 	call(eos), !.
statement_list_L([Line|Lines])	-->  	statement_L(Line), statement_list_L(Lines).

statement_L([])     		--> 	( "\n" ; call(eos)  ), !.
statement_L([L|Ls]) 		--> 	[L], statement_L(Ls).

/*convert into tokens*/
convert_list([], []).
convert_list([L|Ls], Fs) 	:- 	length(L, Len),
    					Len =:= 0,
					convert_list(Ls, Fs).

/* Parser doesn't need comments. line is checked for comment. */
convert_list([L|Ls], Fs) :- 		check_comment(L),
					convert_list(Ls, Fs).

/* save all ascii codes to a list */
convert_list([L|Ls], [F|Fs]) 	:- 	length(L, Len),
    					Len =\= 0,
    					convert_line(L, [], F ),
					convert_list(Ls, Fs).

/* convert ascii list to readable form */
convert_line([], X, CL1)	:-	reverse(X, Y),
    					atom_codes(F, Y),
    					atom_number(F, Num),
    					CL1 = [Num].
    					
/* convert '10' to 10 */
convert_line([], X, CL1)	:-	reverse(X, Y),
    					atom_codes(F, Y),
    					\+ atom_number(F, _),
    					CL1 = [F].
  					

/* save ascii code for everything except space */
convert_line([H|T], X, CL)	:- 	H =\= 32, H =\= 59,
					token_codes(L), not_special(H, L),
					convert_line(T, [H|X], CL).

/* when space convert to readable word */
convert_line([H|T], X, [CL1|CL]) :-	H =:= 32, 
    					reverse(X, Y), 
    					atom_codes(F, Y),
    					atom_number(F, Num),
    					CL1 = Num,
    					convert_line(T, [], CL).

/* when not '10' convert to readable word */
convert_line([H|T], X, [CL1|CL]) :-	H =:= 32, 
    					reverse(X, Y), 
    					atom_codes(F, Y),
    					\+ atom_number(F, _),
    					CL1 = F,
    					convert_line(T, [], CL).


convert_line([H|T], X, [CL1|CL]) :-	H =:= 59, 
    					reverse(X, Y), 
    					atom_codes(F, Y),
    					atom_number(F, Num),
    					CL1 = Num,
    					convert_line(T, [59], CL).

convert_line([H|T], X, [CL1|CL]) :-	H =:= 59, 
    					reverse(X, Y), 
    					atom_codes(F, Y),
    					\+ atom_number(F, _),
    					CL1 = F,
    					convert_line(T, [59], CL).

not_special(_, [])		:-	false.

not_special(X, [H|T]) 		:-	X =\= H,
					not_special(X, T).

not_special(X, [H|_]) 		:-	X =:= H,
					true.

check_comment([H|_]) 		:- 	H =:= 35, true.

/* token ascii codes */
token_codes([59, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 
	     76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
	     87, 88, 89, 90, 97, 98, 99, 100, 101, 102,
	     103, 104, 105, 106, 107, 108, 109, 110, 111,
	     112, 113, 114, 115, 116, 117, 118, 119, 120,
	     121, 122, 123, 124, 125, 48, 49, 50, 51, 52,
	     53, 54, 55, 56, 57, 33, 37, 40, 41, 42, 43,
	     45, 47, 60, 61, 62, 48, 59, 9]).





/*********************************************************************/
Parser Code
/*********************************************************************/

program(P)-->statement_list(P).

statement_list([S,L])-->statement(S),[';'],statement_list(L).
statement_list(S)-->statement(S),[';'].

statement(S)-->assignment(S).
statement(D)-->declaration(D).
statement(P)-->print_statement(P).
statement(I)-->if_statement(I).
statement(W)-->while_statement(W).

print_statement('print'(E))-->['print'],expression(E).
print_statement('print'(C))-->['print'],comparision(C).
print_statement('print'('nl'))-->['print'],['nl'].
print_statement('print'(ID))-->['print'],identifier(ID).
print_statement('print'(S))-->['print'],isString(S).
isString(S)-->[S],{string(S)}.

declaration('int'(ID))--> ['int'], identifier(ID).
declaration('bool'(ID))--> ['bool'], identifier(ID).

assignment('='(L,R))-->identifier(L),['='],righthand(R).
assignment('='(ID,N))--> identifier(ID),['='],expression(N).
assignment('='(ID,N))--> declaration(ID),['='],expression(N).
righthand(R)-->boolean(R).
righthand(R)-->comparision(R).

boolean(true)-->[true].
boolean(false)-->[false].

comparision('=='(E1,E2))-->expression(E1),['=='],expression(E2).
comparision('>='(E1,E2))-->expression(E1),['>='],expression(E2).
comparision('<='(E1,E2))-->expression(E1),['<='],expression(E2).
comparision('<'(E1,E2))-->expression(E1),['<'],expression(E2).
comparision('>'(E1,E2))-->expression(E1),['>'],expression(E2).
comparision('!='(E1,E2))-->expression(E1),['!='],expression(E2).

expression('+'(T,E))-->term(T),['+'],expression(E),!.
expression('-'(T,E))-->term(T),['-'],expression(E),!.
expression('/'(T,E))-->term(T),['/'],expression(E),!.
expression('*'(T,E))-->term(T),['*'],expression(E),!.
expression('%'(T,E))-->term(T),['%'],expression(E),!.
expression(T)-->term(T).

term(T)-->identifier(T).
term(T)-->terminal(T).
terminal(N)-->number(N).
%terminal((N,T))-->number(N),terminal(T).
%terminal(N)-->number(N).

identifier(S)-->[S].
%identifier((L,I))-->letter(L),identifierterm(I).
%identifier(L)-->letter(L).
%identifierterm((L,I))-->letter(L),identifierterm(I).
%identifierterm((N,I))-->number(N),identifierterm(I).
%identifierterm(L)-->letter(L).
%identifierterm(N)-->number(N).

if_statement('if'(S,R))-->['if'], parent_start(S,R).
parent_start(C,R)--> ['('], condition(C),[')'], then_block(R).
parent_end(R)-->then_block(R).
then_block('then'(B,E))--> ['then'], block(B),else_block(E),!.
then_block('then'(B))--> ['then'], block(B).
else_block('else'(B))-->['else'], block(B).

while_statement('while'(B,R))--> ['while'], parent_while_start(B,R).
parent_while_start(C,R)--> ['('], condition(C),[')'], block(R).
%parent_while_end(R)--> [')'], block(R).
block(S)--> ['{'],brace_end(S).
brace_end(S)--> statement_list(S),['}'].

condition(C)--> comparision(C).
condition(B)--> boolean(B).
condition(T)--> terminal(T).

number(N)-->[N], {isnumber(N)}.
isnumber(N):-number(N).


/*********************************************************************/
Interpreter Code
/*********************************************************************/
/* Currently all operations, assignment, if-then-else statements are working */

/* high-level predicate to call the interpreter program. */

/* Currently all operations, assignment, if-then-else statements are working */

/* high-level predicate to call the interpreter program. */

/*T = program(statement_List((statement(declare(int(id(a)))), 
	    statement_List((statement(assign(id(a), expression(add(term(id(1)), expression(term(id(2)))))))))).


eval_Program(program(statement_List(statement(if(condition(term(id(b))), 
	then(statement_List(statement(assign(id(a), expression(add(term(id(a)), expression(term(id(b))))))))),
	else(statement_List((statement(assign(id(a), expression(add(term(id(a)), expression(term(id(b))))))))))))))). 

*/

eval_Program(T) :-   	add_to_env([[x, 5]], [y,20], Env),
		        reduce_Program(T, Env, _).


/* update ENV(empty) for the first identifier */
add_to_env(E, L, Env) :-        length(E, N),
                                N = 0,
                                Env = [L].


/* update ENV, a new identifier, E1 = E0 & {I, V} */
add_to_env(E, [Id, Value], Env_New) :-  length(E, N),
                                	N > 0,
    					look_up(Id, Value, E),
    					delete(E, [Id, _], Env),
	    				append(Env, [[Id,Value]], Env_New), !.

//update ENV, when an identifiers value is updated,  E1 = E0{I, V1} & {I, V2} 
add_to_env(E, [Id, Value], Env) :-  length(E, N),
                                    N > 0,
    				    \+look_up(Id, Value, E),
                                    append(E, [[Id, Value]], Env), !.

/* entry point*/
reduce_Program(program(T), Env, Env_New) :-   reduce_Statement_List(T, Env, Env_New).

reduce_Statement_List(statement_List(T, E), Env, Env_New) :-  reduce_Statement(T, Env, Env_N),
                                                              reduce_Statement_List(E, Env_N, Env_New).
reduce_Statement_List(statement_List(T), Env, Env_New) :-     reduce_Statement(T, Env, Env_New).


/* statement can be of various kinds - */
reduce_Statement(statement(T), Env, Env_New) :-     reduce_assignment(T, _, Env, Env_New).
%reduce_Statement(statement(T), Env, Env_New) :-    reduce_declaration(T, _, Env, Env_New), nl.
reduce_Statement(statement(T), Env, Env_New) :-     reduce_If_Statement(T, Env, Env_New).
reduce_Statement(statement(T), Env, Env_New) :-     reduce_While(T, Env, Env_New).
reduce_Statement(statement(T), Env, Env_New) :-     reduce_print(T, Env, Env_New), !.





/*assign operator*/                                            
reduce_assignment(assign(T, E), R, Env, Env_New) :-  eval_term(T, R, Env, Env), 
						    \+ integer(R),
						    reduce_expression(E, R2, Env, Env_NN),
					    	    add_to_env(Env_NN, [R, R2], Env_New), !.
/*Error handling-assign operator*/
reduce_assignment(assign(T, _), _, _, _) :- 	integer(T), write('Error: Value cannot assigned to an integer'), !.



/*Declare with assignment operator*/  
reduce_declaration(declare(T, E), R, Env, Env_New) :- 	eval_declaration(T,R,Env,Env_N), 
	    										!,
    											eval_terminal(E, R2, Env_N, Env_NN), 
    											add_to_env(Env_NN, [R, R2], Env_New), !.
reduce_declaration(declare(T), R, Env, Env_New) :- eval_declaration(T,R,Env,Env_New), !.
reduce_declaration(declare(T), _, _,_) :- reduce_datatype(T, T1, _, _), integer(T1),  write('Error: Declaration cannot take integer value'), !.

/*Declare without assignment operator*/                                               
eval_declaration(T, R, Env, Env_New) :- reduce_datatype(T, T1,D, Env), eval_term(T1, R, 0, _),\+ integer(R), add_to_env(Env, [R,D], Env_New), !.

/*Error handling-Declare operator*/ 

reduce_datatype(int(T),T, D,_):- D = 0.
reduce_datatype(bool(T),T,D,_):- D= false.



/* eval for only if-then condition */
reduce_If_Statement(if(If, Then), Env, Env_New) :-    eval_Condition(If, Bool, Env, Env_N),
                                                	!,
                                                	is_true(Bool),
                                                	reduce_Then(Then, Env_N, Env_New).
/* eval for if condition is true */
reduce_If_Statement('if'(If, Then, _), Env, Env_New) :- eval_Condition(If, Bool, Env, Env_N),
                                                        !,
                                                        is_true(Bool),
                                                        reduce_Then(Then, Env_N, Env_New).
/* eval for if condition is false */
reduce_If_Statement('if'(If, _, Else), Env, Env_New)  :-  eval_Condition(If, Bool, Env, Env_N),
                                                        !,
                                                        \+ is_true(Bool),
                                                        reduce_Else(Else, Env_N, Env_New).

reduce_Then('then'(T), Env, Env_New) :-          	reduce_Statement_List(T, Env, Env_New).

reduce_Else('else'(T), Env, Env_New) :-          	reduce_Statement_List(T, Env, Env_New).

%while

reduce_While(while(While,Then), Env, Env_NewRepeat) :- 	reduce_expression(While, R, Env, Env_N), 
						   	R >= 1, 
						   	reduce_Then(Then, Env_N, Env_New), 
						   	reduce_While(while(While,Then), Env_New, Env_NewRepeat).

reduce_While(while(While, _), Env, Env_New) :- 	reduce_expression(While, R, Env, Env_New), R=<0.

reduce_print(print(T), Env, Env_New) :- eval_print(T, _, Env, Env_New), !.


/*condition for if(), various types */
eval_Condition(condition(T), R, Env, Env_New) :-    	eval_boolean(T, R, Env, Env_New).
eval_Condition(condition(T), R, Env, Env_New) :-    	eval_compareEquals(T, R, Env, Env_New).
eval_Condition(condition(T), R, Env, Env_New) :-    	eval_compareLesser(T, R, Env, Env_New).
eval_Condition(condition(T), R, Env, Env_New) :-    	eval_compareGreater(T, R, Env, Env_New).
eval_Condition(condition(T), R, Env, Env_New) :-    	eval_compareLesserequal(T, R, Env, Env_New).
eval_Condition(condition(T), R, Env, Env_New) :-    	eval_compareGreaterEqual(T, R, Env, Env_New).
eval_Condition(condition(T), R, Env, Env_New) :-    	eval_compareNotEqual(T, R, Env, Env_New).


eval_boolean(term(T), Value, Env, Env_New) :-  eval_term(T, Value, Env, Env_New),
    										write("value exist").
												
eval_boolean(term(T), _, Env, _) :- 	\+ eval_term(T, _, Env, _),
										write("Error: Value do not exist").
													

/* Comparison operators:  '==', ‘<’, ‘>’, ‘<=’, ‘>=’, ‘!=’ */

eval_compareEquals(compareEquals(T, E), R, Env, Env_New) :- eval_terminal(T, R, 0, Env),
                                                	      	!,
                                                	      	reduce_expression(E, R2, Env, Env_NN),
                                                	      	add_to_env(Env_NN, [R, R2], Env_New),
    														write(Env_New),
    														nl, !.
                                                		
eval_compareLesser('compareLesser'(E, E1), R, Env, Env_New) :- 	reduce_expression(E, R1, Env, Env_N),!,
								reduce_expression(E1, R2, Env_N, Env_New), 
								eval(R1<R2, R), !.

eval_compareGreater('compareGreater'(E, E1), R, Env, Env_New) :- 	reduce_expression(E, R1, Env, Env_N),!,
									reduce_expression(E1, R2, Env_N, Env_New), 
									eval(R1>R2, R), !.

eval_compareLesserequal('compareLesserequal'(E, E1), R, Env, Env_New) :- 	reduce_expression(E, R1, Env, Env_N),!,
										reduce_expression(E1, R2, Env_N, Env_New), 
										eval(R1=<R2, R), !.

eval_compareGreaterEqual('compareGreaterEqual'(E, E1), R, Env, Env_New) :- 	reduce_expression(E, R1, Env, Env_N),!,
										reduce_expression(E1, R2, Env_N, Env_New), 
										eval(R1>=R2, R), !.

eval_compareNotEqual('compareNotEqual'(E, E1), R, Env, Env_New) :- 	reduce_expression(E, R1, Env, Env_N),!,
									reduce_expression(E1, R2, Env_N, Env_New),
									eval(R1\=R2, R), !. 


eval_print(T, R, Env, Env_New) :- reduce_expression(T, R, Env, Env_New),
				  write(R), 
    				nl, !.

check(Exp):- Exp.
eval(Exp, R):- check(Exp), R is 1.
eval(Exp, R):- \+ check(Exp), R is 0.


reduce_expression(expression(T), R, Env, Env_New) :- eval_expression(T, R, Env, Env_New).
reduce_expression(expression(T), R, Env, Env_New) :- eval_terminal(T, R, Env, Env_New).

/* evaluation function for operators */
eval_expression(add(T, E), R, Env, Env_New) :-  	eval_terminal(T, R1, Env, Env_N),
                                                	!,
                                               	 	reduce_expression(E, R2, Env_N, Env_New),
                                                	R is R1 + R2.

eval_expression(subtract(T, E), R, Env, Env_New) :-   	eval_terminal(T, R1, Env, Env_N),
                                                		!,
                                                		reduce_expression(E, R2, Env_N, Env_New),
                                                		R is R1 - R2, !.

eval_expression(multiply(T, E), R, Env, Env_New) :- eval_terminal(T, R1, Env, Env_N),
                                                	!,
                                                	reduce_expression(E, R2, Env_N, Env_New),
                                                	R is R1 * R2, !.


eval_expression(divide(T, E), R, Env, Env_New) :-   eval_terminal(T, R1, Env, Env_N),
                                                	reduce_expression(E, R2, Env_N, Env_New),
    												check_divion_by_zero(R2),	
                                                	R is R1 / R2, !.

eval_expression(divide(T, E), R, Env, Env_New) :-   eval_terminal(T, R, Env, Env_N),
                                                	!,
                                                   reduce_expression(E, R2, Env_N, Env_New),
					    	    \+ check_divion_by_zero(R2),	
						    write("Error, division by zero"), !.

/* eval func for mod */
eval_expression(modulo(T, E), R, Env, Env_New)  :-    	eval_terminal(T, R1, Env, Env_N),
                                                      	!,
                                                		reduce_expression(E, R2, Env_N, Env_New),
                                                		R is mod(R1,R2), !.


/* eval func for terminal */
eval_terminal(term(T), R, Env, Env_New) :-        eval_term(T, R, Env, Env_New).


eval_term(id(T), T, Env, Env) :-   integer(T).

/* when term is identifier */
eval_term(id(T), T, 0, _) :-		\+ integer(T).

eval_term(id(T), R, Env, Env) :-	\+ integer(T),
                                     		look_up(T, Value, Env),
                                     		R = Value.

eval_term()

check_divion_by_zero(Divisor) :- Divisor =\= 0.     					


/* look-up for variables in env */

look_up(Id, Value, [H|_]) :-    look_Id(Id, Value, H).
                               
look_up(Id, Value, [H|T]) :-    	\+ look_Id(Id, _, H),
                                look_up(Id, Value, T).

look_Id(Id, Value, [Id, Value]).

is_true(Bool) :- Bool = true.





