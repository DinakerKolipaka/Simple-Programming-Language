program(program(P))-->statement_list(P).
statement(statement(S))-->assignment(S).
statement(statement(D))-->declaration(D).
statement(statement(P))-->print_statement(P).
statement(statement(I))-->if_statement(I).
statement(statement(W))-->while_statement(W).



assignment(assign('='(L,R)))-->identifier(L),['='],righthand(R).
assignment(assign('='(L,R)))-->declaration(L),['='],righthand(R).
righthand(righthand(R))-->comparision(R).
righthand(righthand(R))-->expression(R).
righthand(righthand(R))-->boolean(R).


boolean(true)-->[true].
boolean(false)-->[false].

declaration(declare('int'(ID)))--> ['int'], identifier(ID).
declaration(declare(bool(ID)))--> ['bool'], identifier(ID).

comparision(compare('=='(E1,E2)))-->expression(E1),['=='],expression(E2).
comparision(compare('>='(E1,E2)))-->expression(E1),['>='],expression(E2).
comparision(compare('<='(E1,E2)))-->expression(E1),['<='],expression(E2).
comparision(compare('<'(E1,E2)))-->expression(E1),['<'],expression(E2).
comparision(compare('>'(E1,E2)))-->expression(E1),['>'],expression(E2).
comparision(compare('!='(E1,E2)))-->expression(E1),['!='],expression(E2).

expression(expression('+'(T,E)))-->term(T),['+'],expression(E),!.
expression(expression('-'(T,E)))-->term(T),['-'],expression(E),!.
expression(expression('/'(T,E)))-->term(T),['/'],expression(E),!.
expression(expression('*'(T,E)))-->term(T),['*'],expression(E),!.
expression(expression('%'(T,E)))-->term(T),['%'],expression(E),!.
expression(expression(T))-->term(T).

term(term(T))-->identifier(T).
term(term(T))-->terminal(T).

terminal(et(N))-->number(N).

condition('condition'(C))--> comparision(C).
condition('condition'(B))--> boolean(B).
condition('condition'(T))--> terminal(T).

number('number'(0))-->[0].
number('number'(1))-->[1].
number('number'(2))-->[2].
number('number'(3))-->[3].
number('number'(4))-->[4].
number('number'(5))-->[5].
number('number'(6))-->[6].
number('number'(7))-->[7].
number('number'(8))-->[8].
number('number'(9))-->[9].
number('number'(N))-->[N],{isnumber(N)}.
isnumber(N):-number(N).


identifier('id'(S))-->[S].

print_statement('print'(E))-->['print'],expression(E).
print_statement('print'(C))-->['print'],comparision(C).
print_statement('print'('nl'))-->['print'],['nl'].
print_statement('print'(ID))-->['print'],identifier(ID).
print_statement('print'(S))-->['print'],isString(S).
isString(S)-->[S],{string(S)}.

if_statement('if'(S,R))-->['if'], parent_start(S,R).
parent_start(parent_start(C,R))--> ['('], condition(C), parent_end(R).
parent_end(parent_end(R))--> [')'], then_block(R).
then_block('then'(B,E))--> ['then'], block(B),else_block(E),!.
then_block('then'(B))--> ['then'], block(B).
else_block('else'(B))-->['else'], block(B).
block(block(S))--> ['{'],brace_end(S).
brace_end(brace_end(S))--> statement(S),['}'].


statement_list('statement_List'(S,L))-->statement(S),[';'],statement_list(L).
statement_list('statement_List'(S))-->statement(S),[';'].

while_statement('while'(B))--> ['while'], parent_while_start(B).
parent_while_start(parent_while_start(C,R))--> ['('], condition(C), parent_while_end(R).
parent_while_end(parent_while_end(R))--> [')'], block(R).
