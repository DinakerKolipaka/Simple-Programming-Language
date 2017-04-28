program(program(P))-->statement_list(P).

statement_list('statement_List'((S,L)))-->statement(S),[';'],statement_list(L),!.
statement_list('statement_List'(S))-->statement(S),[';'].

statement(statement(S))-->assignment(S).
statement(statement(D))-->declaration(D).
statement(statement(P))-->print_statement(P).
statement(statement(I))-->if_statement(I).
statement(statement(W))-->while_statement(W).

assignment(assign(L,R))-->identifier(L),['='],righthand(R).
assignment(assign(L,R))-->declaration(L),['='],righthand(R).
righthand(R)-->comparision(R).
righthand(R)-->expression(R).
righthand(R)-->boolean(R).

declaration(declare('int'(ID)))--> ['int'], identifier(ID).
declaration(declare('bool'(ID)))--> ['bool'], identifier(ID).

boolean(true)-->[true].
boolean(false)-->[false].

comparision(compareequals(E1,E2))-->expression(E1),['=='],expression(E2).
comparision(comparegreaterequal(E1,E2))-->expression(E1),['>='],expression(E2).
comparision(comparelesserequal(E1,E2))-->expression(E1),['<='],expression(E2).
comparision(comparelesser(E1,E2))-->expression(E1),['<'],expression(E2).
comparision(comparegreater(E1,E2))-->expression(E1),['>'],expression(E2).
comparision(comparenotequal(E1,E2))-->expression(E1),['!='],expression(E2).

expression(expression('add'(T,E)))-->term(T),['+'],expression(E),!.
expression(expression('subtract'(T,E)))-->term(T),['-'],expression(E),!.
expression(expression('divide'(T,E)))-->term(T),['/'],expression(E),!.
expression(expression('multiply'(T,E)))-->term(T),['*'],expression(E),!.
expression(expression('modulo'(T,E)))-->term(T),['%'],expression(E),!.
expression(expression(T))-->term(T).


identifier('id'(S))-->[S].

term(term(T))-->terminal(T),!.
terminal(et(N))-->number(N).
term(term(T))-->identifier(T).

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
number('number'(N))-->[N],{isnumber(N)}.
isnumber(N):-number(N).

condition('condition'(C))--> comparision(C).
condition('condition'(B))--> boolean(B).
condition('condition'(T))--> terminal(T).


print_statement('print'(E))-->['print'],expression(E).
print_statement('print'(C))-->['print'],comparision(C).
print_statement('print'('nl'))-->['print'],['nl'].
print_statement('print'(ID))-->['print'],identifier(ID).
print_statement('print'(S))-->['print'],isString(S).
isString(S)-->[S],{string(S)}.


if_statement('if'(S,R))-->['if'], parent_start(S,R).
parent_start(C,R)--> ['('], condition(C),[')'], then_block(R).
parent_end(R)-->then_block(R).
then_block('then'(B))--> ['then'], block(B).
then_block('then'(B,E))--> ['then'], block(B),else_block(E),!.
else_block('else'(B))-->['else'], block(B).
block(S)--> ['{'],brace_end(S).
brace_end(S)--> statement_list(S),['}'].


while_statement('while'(B))--> ['while'], parent_while_start(B).
parent_while_start(C,R)--> ['('], condition(C), parent_while_end(R).
parent_while_end(R)--> [')'], block(R).
