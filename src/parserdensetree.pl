keywords(X):-X=[if,then,else,while,true,false].

program(P)-->statement_list(P),!.

statement_list(A,B,C,L)-->statement(A,B,C),[';'],statement_list(L),!.
statement_list(S)-->statement(A.B,C),[';'],!.

statement(S,A,B)-->assignment(S,A,_).
statement(D,A,B)-->declaration(D,A,B).
statement(P)-->print_statement(P).
statement(I)-->if_statement(I).
statement(W)-->while_statement(W).

assignment(L,R,_)-->identifier(L),['='],righthand(R),!.
righthand(R)-->comparision(R).
righthand(R)-->expression(R).
righthand(R)-->boolean(R).

declaration(('int'(ID)),T,_)--> ['int'], identifier(ID),['='],term(T),!.
declaration(('bool'(ID)),T,_)--> ['bool'], identifier(ID),['='],term(T),!.
declaration('int'(ID),_,_)--> ['int'], identifier(ID).
declaration('bool'(ID),_,_)--> ['bool'], identifier(ID).


boolean(true)-->[true].
boolean(false)-->[false].

comparision(E1,E2)-->expression(E1),['=='],expression(E2),!.
comparision(E1,E2)-->expression(E1),['>='],expression(E2).
comparision(E1,E2)-->expression(E1), ['<='],expression(E2).
comparision(E1,E2)-->expression(E1),['>'],expression(E2),!.
comparision(E1,E2)-->expression(E1),['<'],expression(E2),!.
comparision(E1,E2)-->expression(E1),['!='],expression(E2),!.

expression('add'(T,E))-->term(T),['+'],expression(E),!.
expression('subtract'(T,E))-->term(T),['-'],expression(E),!.
expression('divide'(T,E))-->term(T),['/'],expression(E),!.
expression('multiply'(T,E))-->term(T),['*'],expression(E),!.
expression('modulo'(T,E))-->term(T),['%'],expression(E),!.
expression(T)-->term(T).


term(T)-->boolean(T),!.

term(T)-->identifier(T),!.
term(T)-->terminal(T).
terminal(N)-->number(N).


%identifier('id'(S))-->[S], !,{\+iskey(S)},!.
identifier('id'(S))-->[S],{iskey(S)}.


iskey(K):-keywords(X),\+member(K,X).
iskey(K):-keywords(X),member(K,X),false.
%writeerror:-write('Error'),false.

number(N)-->[N],{isnumber(N)}.
isnumber(N):-number(N).

tonumber(X,Y):-term_to_atom(Y,X).
condition(C)--> comparision(C).
condition(B)--> boolean(B).
condition(T)--> term(T).


print_statement(E)-->['print'],expression(E),!.
print_statement(C)-->['print'],comparision(C).
print_statement('nl')-->['print'],['nl'].
print_statement(ID)-->['print'],identifier(ID).
print_statement(S)-->['print'],isString(S).
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


tokenWrite(T,P) :- program(T,P,_),
              open('../data/intermediate1.txt', write, Stream),
              write(Stream,T),nl(Stream),close(Stream).
