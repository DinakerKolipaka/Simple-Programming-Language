program('program'(P))-->statement_list(P).
statement('statement'(S))-->assignment(S).
statement('statement'(D))-->declaration(D).
statement('statement'(P))-->print_statement(P).
statement('statement'(I))-->if_statement(I).
statement('statement'(W))-->while_statement(W).



assignment('='(L,R))-->identifier(L),['='],righthand(R).
assignment('='(L,R))-->declaration(L),['='],righthand(R).
righthand(R)-->comparision(R).
righthand(R)-->expression(R).
righthand(R)-->boolean(R).


boolean(true)-->[true].
boolean(false)-->[false].

declaration('int'(ID))--> ['int'], identifier(ID).
declaration('bool'(ID))--> ['bool'], identifier(ID).

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
terminal('et'(N))-->number(N).
%terminal((N,T))-->number(N),terminal(T).
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

%identifier((L,I))-->letter(L),identifierterm(I),!.
%identifier(L)-->letter(L).
%identifierterm((L,I))-->letter(L),identifierterm(I).
%identifierterm((N,I))-->number(N),identifierterm(I).
%identifierterm(L)-->letter(L).
%identifierterm(N)-->number(N).

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


statement_list('statement_List'(S,L))-->statement(S),[';'],statement_list(L).
statement_list('statement_List'(S))-->statement(S),[';'].

while_statement('while'(B))--> ['while'], parent_while_start(B).
parent_while_start((C,R))--> ['('], condition(C), parent_while_end(R).
parent_while_end((R))--> [')'], block(R).
