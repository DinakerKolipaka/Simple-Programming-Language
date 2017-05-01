
# Lexical Analysis Code
# Author: Manju Bisht
# Version: 2.2
# Date: 4/29/2017

# Library for read/write a file
- use_module(library(pio)).
# library for dcg grammar
- use_module(library(dcgbasics)).


eos([], []).

statement_list_L([])          	--> 	call(eos), !.
statement_list_L([Line|Lines])	-->  	statement_L(Line), statement_list_L(Lines).

statement_L([])     		--> 	( "\n" ; call(eos)  ), !.
statement_L([L|Ls]) 		--> 	[L], statement_L(Ls).

# convert into tokens
convert_list([], []).
convert_list([L|Ls], Fs) 	:- 	length(L, Len),
    					Len =:= 0,
					convert_list(Ls, Fs).

# Parser doesnt need comments. line is checked for comment. 
convert_list([L|Ls], Fs) :- 		check_comment(L),
					convert_list(Ls, Fs).

# save all ascii codes to a list 
convert_list([L|Ls], [F|Fs]) 	:- 	length(L, Len),
    					Len =\= 0,
    					convert_line(L, [], F ),
					convert_list(Ls, Fs).

# convert ascii list to readable form 
convert_line([], X, CL1)	:-	reverse(X, Y),
    					atom_codes(F, Y),
    					atom_number(F, Num),
    					CL1 = [Num].
    					
# convert '10' to 10 
convert_line([], X, CL1)	:-	reverse(X, Y),
    					atom_codes(F, Y),
    					\+ atom_number(F, _),
    					CL1 = [F].
  					

# save ascii code for everything except space 
convert_line([H|T], X, CL)	:- 	H =\= 32, H =\= 59,
					token_codes(L), not_special(H, L),
					convert_line(T, [H|X], CL).

# when space convert to readable word 
convert_line([H|T], X, [CL1|CL]) :-	H =:= 32, 
    					reverse(X, Y), 
    					atom_codes(F, Y),
    					atom_number(F, Num),
    					CL1 = Num,
    					convert_line(T, [], CL).

# when not '10' convert to readable word 
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

# token ascii codes 
token_codes([59, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 
	     76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
	     87, 88, 89, 90, 97, 98, 99, 100, 101, 102,
	     103, 104, 105, 106, 107, 108, 109, 110, 111,
	     112, 113, 114, 115, 116, 117, 118, 119, 120,
	     121, 122, 123, 124, 125, 48, 49, 50, 51, 52,
	     53, 54, 55, 56, 57, 33, 37, 40, 41, 42, 43,
	     45, 47, 60, 61, 62, 48, 59, 9]).




# Parser Code
# Author: Vidhi Patel, Dinaker Prakash Kolipaka
# Version: 7.2
# Date: 4/30/2017


keywords(X):-X=[if,then,else,while,true,false].
program(program(P))-->statement_list(P),!.

statement_list('statement_List'(S,L))-->statement(S),[';'],statement_list(L),!.
statement_list('statement_List'(S))-->statement(S),[';'],!.

statement(statement(S))-->assignment(S).
statement(statement(D))-->declaration(D).
statement(statement(P))-->print_statement(P).
statement(statement(I))-->if_statement(I).
statement(statement(W))-->while_statement(W).

assignment(assign(L,R))-->identifier(L),['='],righthand(R),!.
righthand(R)-->comparision(R).
righthand(R)-->expression(R).
righthand(R)-->boolean(R).

declaration(declare(('int'(ID)),T))--> ['int'], identifier(ID),['='],term(T),!.
declaration(declare(('bool'(ID)),T))--> ['bool'], identifier(ID),['='],term(T),!.
declaration(declare('int'(ID)))--> ['int'], identifier(ID).
declaration(declare('bool'(ID)))--> ['bool'], identifier(ID).


boolean(true)-->[true].
boolean(false)-->[false].

comparision(compareEquals(E1,E2))-->expression(E1),['=='],expression(E2),!.
comparision(compareGreaterEqual(E1,E2))-->expression(E1),['>='],expression(E2).
comparision(compareLesserEqual(E1,E2))-->expression(E1), ['<='],expression(E2).
comparision(compareGreater(E1,E2))-->expression(E1),['>'],expression(E2),!.
comparision(compareLesser(E1,E2))-->expression(E1),['<'],expression(E2),!.
comparision(compareNotEqual(E1,E2))-->expression(E1),['!='],expression(E2),!.

expression(expression('add'(T,E)))-->term(T),['+'],expression(E),!.
expression(expression('subtract'(T,E)))-->term(T),['-'],expression(E),!.
expression(expression('divide'(T,E)))-->term(T),['/'],expression(E),!.
expression(expression('multiply'(T,E)))-->term(T),['*'],expression(E),!.
expression(expression('modulo'(T,E)))-->term(T),['%'],expression(E),!.
expression(expression(T))-->term(T).


term(term(T))-->boolean(T),!.

term(term(T))-->identifier(T),!.
term(term(T))-->terminal(T).
terminal(et(N))-->number(N).


%identifier('id'(S))-->[S], !,{\+iskey(S)},!.
identifier('id'(S))-->[S],{iskey(S)}.


iskey(K):-keywords(X),\+member(K,X).
iskey(K):-keywords(X),member(K,X),write('Error'),false.
%writeerror:-write('Error'),false.

number('number'(N))-->[N],{isnumber(N)}.
isnumber(N):-number(N).

tonumber(X,Y):-term_to_atom(Y,X).
condition('condition'(C))--> comparision(C).
condition('condition'(B))--> boolean(B).
condition('condition'(T))--> term(T).


print_statement('print'(E))-->['print'],expression(E),!.
print_statement('print'(C))-->['print'],comparision(C).
print_statement('print'('nl'))-->['print'],['nl'].
print_statement('print'(ID))-->['print'],identifier(ID).
print_statement('print'(S))-->['print'],isString(S).
isString(S)-->[S],{string(S)}.


if_statement('if'(C,I,E))-->['if'], parent_start(C,I,E),!.
if_statement('if'(C,I))-->['if'], parent_start(C,I).
parent_start(C,I,E)--> ['('], condition(C),[')'], then_block(I),else_block(E),!.
parent_start(C,I)--> ['('], condition(C),[')'], then_block(I),!.
then_block('then'(B))--> ['then'], block(B).
else_block('else'(B))-->['else'], block(B).
block(S)--> ['{'],brace_end(S).
brace_end(S)--> statement_list(S),['}'].


while_statement('while'(B,R))--> ['while'], ['('], condition(B), [')'], block(R),!.
%parent_while_start((C,R))--> parent_while_end(R).
%parent_while_end(R)--> .


tokenWrite(Token) :- program(T,Token,_),
              open('data/intermediate1.imc', write, Stream),
              write(Stream,T),nl(Stream),close(Stream).
