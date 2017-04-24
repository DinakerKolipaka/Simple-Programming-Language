program('program'(P))-->statement_list(P).
statement('statement'(S))-->assignment(S).
statement('statement'(D))-->declaration(D).
statement('statement'(P))-->print_statement(P).
statement('statement'(I))-->if_statement(I).
statement('statement'(W))-->while_statement(W).

statement_list([S|L])-->statement(S),statement_list(L).
statement_list([])-->[].
%statement_list([S])-->statement([S]).

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

