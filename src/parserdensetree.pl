program(program(P))-->statement_list(P).

statement_list('statement_List'([S,L]))-->statement(S),[';'],statement_list(L).
statement_list('statement_List'(S))-->statement(S),[';'].

statement(statement(S))-->assignment(S).
statement(statement(D))-->declaration(D).
statement(statement(P))-->print_statement(P).
statement(statement(I))-->if_statement(I).
statement(statement(W))-->while_statement(W).

assignment(assign('='(L,R)))-->identifier(L),['='],righthand(R).
assignment(assign('='(L,R)))-->declaration(L),['='],righthand(R).
righthand(R)-->comparision(R).
righthand(R)-->expression(R).
righthand(R)-->boolean(R).

declaration(declare('int'(ID)))--> ['int'], identifier(ID).
declaration(declare(bool(ID)))--> ['bool'], identifier(ID).

boolean(true)-->[true].
boolean(false)-->[false].

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

number('number'(N))-->[N],{isnumber(N)}.
isnumber(N):-number(N).

condition('condition'(C))--> comparision(C).
condition('condition'(B))--> boolean(B).
condition('condition'(T))--> terminal(T).

identifier('id'(S))-->[S].

print_statement('print'(E))-->['print'],expression(E).
print_statement('print'(C))-->['print'],comparision(C).
print_statement('print'('nl'))-->['print'],['nl'].
print_statement('print'(ID))-->['print'],identifier(ID).
print_statement('print'(S))-->['print'],isString(S).
isString(S)-->[S],{string(S)}.

if_statement('if'(S,R))-->['if'], parent_start(S,R).
parent_start(C,R)--> ['('], condition(C), parent_end(R).
parent_end(R)--> [')'], then_block(R).
then_block('then'(B,E))--> ['then'], block(B),else_block(E),!.
then_block('then'(B))--> ['then'], block(B).
else_block('else'(B))-->['else'], block(B).
block(S)--> ['{'],brace_end(S).
brace_end(S)--> statement(S),['}'].

while_statement('while'(B))--> ['while'], parent_while_start(B).
parent_while_start(C,R)--> ['('], condition(C), parent_while_end(R).
parent_while_end(R)--> [')'], block(R).
