ii/*********************************************************************/
Lexer Code
/*********************************************************************/

%read from statements from file
eos([], []).

statement_list([])          	--> 	call(eos), !.
statement_list([Line|Lines])	-->  	statement(Line), statement_list(Lines).

statement([])     		--> 	( ";" ; call(eos) ; "\n" ; call(eos)  ), !.
statement([L|Ls]) 		--> 	[L], statement(Ls).

%convert into tokens
convert_list([], []).
convert_list([L|Ls], Fs) 	:- 	length(L, Len),
    					Len =:= 0,
					convert_list(Ls, Fs).

convert_list([L|Ls], Fs) :- 		check_comment(L),
					convert_list(Ls, Fs).

convert_list([L|Ls], [F|Fs]) 	:- 	length(L, Len),
    					Len =\= 0,
    					convert_line(L, [], F ),
					convert_list(Ls, Fs).

convert_line([], X, CL1)	:-	reverse(X, Y),
    					atom_codes(F, Y),
    					CL1 = [F].
    					

convert_line([H|T], X, CL)	:- 	H =\= 32, 
					token_codes(L), not_special(H, L),
					convert_line(T, [H|X], CL).

convert_line([H|T], X, [CL1|CL]) :-	H =:= 32, 
    					reverse(X, Y), 
    					atom_codes(F, Y),
    					CL1 = F, 
    					convert_line(T, [], CL).

not_special(_, [])		:-	false.

not_special(X, [H|T]) 		:-	X =\= H,
					not_special(X, T).

not_special(X, [H|_]) 		:-	X =:= H,
					true.

check_comment([H|_]) 		:- 	H =:= 35, true.

token_codes([65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 
	     76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
	     87, 88, 89, 90, 97, 98, 99, 100, 101, 102,
	     103, 104, 105, 106, 107, 108, 109, 110, 111,
	     112, 113, 114, 115, 116, 117, 118, 119, 120,
	     121, 122, 123, 124, 125, 48, 49, 50, 51, 52,
	     53, 54, 55, 56, 57, 33, 37, 40, 41, 42, 43,
	     45, 47, 60, 61, 62]).






/*********************************************************************/
Parser Code
/*********************************************************************/

program(P)-->statement_list(P).

statement(S)-->assignment(S).
statement(D)-->declaration(D).
statement(P)-->print_statement(P).
statement(I)-->if_statement(I).
statement(W)-->while_statement(W).

statement_list([S|L])-->statement(S),statement_list(L).
statement_list([])-->[].
%statement_list([S])-->statement([S]).
seq([]) --> [].
seq([E|Es]) --> statement(E), seq(Es).
seqq([]) --> [].
seqq([Es|Ess]) --> seq(Es), seqq(Ess).

print_statement('print'(E))-->['print'],expression(E).
print_statement('print'(C))-->['print'],comparision(C).
print_statement('print'('nl'))-->['print'],['nl'].
print_statement('print'(ID))-->['print'],identifier(ID).
print_statement('print'(S))-->['print'],isString(S).
isString(S)-->[S],{string(S)}.
%statement_list((list([a,b]),list([c,d])), Ls).

declaration('int'(ID))--> ['int'], identifier(ID).
declaration('bool'(ID))--> ['bool'], identifier(ID).
% Rule 4
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

expression('+'(T,E))-->term(T),['+'],!,expression(E).
expression('-'(T,E))-->term(T),['-'],!,expression(E).
expression('/'(T,E))-->term(T),['/'],!,expression(E).
expression('*'(T,E))-->term(T),['*'],!,expression(E).
expression('%'(T,E))-->term(T),['%'],!,expression(E).
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
parent_start(C,R)--> ['('], condition(C), parent_end(R).
parent_end(R)--> [')'], then_block(R).
then_block('then'(B,E))--> ['then'], block(B),else_block(E),!.
then_block('then'(B))--> ['then'], block(B).
else_block('else'(B))-->['else'], block(B).
block(S)--> ['{'],brace_end(S).
brace_end(S)--> statement_list(S),['}'].

while_statement('while'(B,R))--> ['while'], parent_while_start(B,R).
parent_while_start(C,R)--> ['('], condition(C), parent_while_end(R).
parent_while_end(R)--> [')'], block(R).

condition(C)--> comparision(C).
condition(B)--> boolean(B).
condition(T)--> terminal(T).

number(0)-->[0].
number(1)-->[1].
number(2)-->[2].
number(3)-->[3].
number(4)-->[4].
number(5)-->[5].
number(6)-->[6].
number(7)-->[7].
number(8)-->[8].
number(9)-->[9].
number(N)-->[N], {isnumber(N)}.
isnumber(N):-number(N).
letter(a)-->[a].
letter(b)-->[b].
letter(c)-->[c].
letter(d)-->[d].
letter(e)-->[e].
letter(f)-->[f].
letter(g)-->[g].
letter(h)-->[h].
letter(i)-->[i].
letter(j)-->[j].
letter(k)-->[k].
letter(l)-->[l].
letter(m)-->[m].
letter(n)-->[n].
letter(o)-->[o].
letter(p)-->[p].
letter(q)-->[q].
letter(r)-->[r].
letter(s)-->[s].
letter(t)-->[t].
letter(u)-->[u].
letter(v)-->[v].
letter(w)-->[w].
letter(x)-->[x].
letter(y)-->[y].
letter(z)-->[z].






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

