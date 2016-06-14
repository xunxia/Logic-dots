% generate the column number list. eg,[1,2,3,4,5].
column(0,[]).
column(Len, [Len|NoList]):-
	Temp1 is Len-1,
	column(Temp1,NoList).

column_check([],TempRes,TempRes).
column_check([_/C|Plan],ZeroList,SumList):-
	add(ZeroList,C,TempRes),
	column_check(Plan,TempRes,SumList).

absoluSum([],Temp,Sum):-
	Sum is Temp.
absoluSum([H|T],Temp,Sum):-
	H1 is abs(H),
	Temp1 is H1+Temp,
	absoluSum(T,Temp1,Sum).

generateRow(_,0,[]).
generateRow(X,Num,[X|List]):-
	Num>0,
	N1 is Num-1,
	generateRow(X,N1,List).

compareCR([],[],[]).
compareCR([H|T],[H1|T1],[Temp|Result]):-
	Temp is abs(H-H1),
	compareCR(T,T1,Result).

add([H|T],1,[H1|T]):-
	H1 is H+1,!.
add([H|T],Index,[H|Result]):-
	In1 is Index-1,
	add(T,In1,Result).

sub([H|T],1,[H1|T]):-
	H1 is H-1,!.
sub([H|T],Index,[H|Result]):-
	In1 is Index-1,
	sub(T,In1,Result).

check(Column,Row):-
	sum_list(Row,X),sum_list(Column,Y),is(X,Y),
	length(Row,R),length(Column,C),X<R*C;X=R*C,
	(   member(0,Row)
	->  \+member(R,Column);
	false),
	(   member(0,Column)
	->  \+member(C,Row);
	false).

goal(Plan,Column_Value,Sum):-
	length(Column_Value,C),
	generateRow(0,C,ZeroList),
	column_check(Plan,ZeroList,SumList),
	compareCR(Column_Value,SumList,List),
	sum_list(List,Sum).
	% absoluSum(List,0,Sum).

compareCheck([],[]).
compareCheck([HC|TC],[HZ|TZ]):-
	HZ =< HC,
	compareCheck(TC,TZ).

permute([ ],[ ]) :- !.
permute(L,[X|R]) :-
    omit(X,L,M),
    permute(M,R).

omit(H,[H|T],T).
omit(X,[H|L],[H|R]) :-
    omit(X,L,R).

next(R,C,L):-%possibility of next line
	\+is(R,C),
	generateRow(1,R,T1),
	generateRow(0,C-R,T2),
	append(T1,T2,T3),
	setof(Temp,permute(T3,Temp),L).

next(R,C,L):-%possibility of next line
	is(R,C),
	R>0,
	generateRow(1,R,T1),
	setof(Temp,permute(T1,Temp),L)
	.
next(R,C,L):-%possibility of next line
	is(R,C),
	R=0,
	generateRow(0,R,T1),
	setof(Temp,permute(T1,Temp),L).

nextInd([],[]).
nextInd([H|T],[TList|R]):-
	findall(Ind,nth1(Ind,H,1),TList),
	nextInd(T,R).

geneNode(_,[],_,[]).
geneNode(Row_No,[ColNo|CList],Blocks,[Row_No/ColNo|R]):-
	\+ member(Row_No/ColNo,Blocks),
	geneNode(Row_No,CList,Blocks,R).

dupliCheck([]).
dupliCheck([H|T]):-
	\+ member(H,T),
	dupliCheck(T).

fitColu(Column_Value,Plan):-
	dupliCheck(Plan),
	length(Column_Value,C),
	generateRow(0,C,ZeroList),
	column_check(Plan,ZeroList,SumList),
	compareCheck(Column_Value,SumList).

%single_row_move(_,_,0,_,_,[]).
%single_row_move(ColuList,Row_No,
%				Row_Value,Colu_Value,Blocks,
%				[Row_No/Colu_No|RowList]):-
	%column(Len,ColuListTemp),

	%reverse(ColuListTemp,ColuList),
	%max_member(Max,Colu_Value),
	%nth1(Ind,Colu_Value,Max),
	%nth1(Ind,ColuList,Colu_No),
	%sub(Colu_Value,Ind,CVTemp),

	%member(Colu_No,ColuList),

single_row_move(Row_No,Row_Value,Colu_Value,Blocks,RowList):-
	length(Colu_Value,Len),
	next(Row_Value,Len,PossibleList),
	nextInd(PossibleList,ColInd),
	member(Colu_NoList,ColInd),
	geneNode(Row_No,Colu_NoList,Blocks,RowList).

	% \+ member(Row_No/Colu_No,Blocks),
	%delete(ColuList,Colu_No,ColuListTemp),
	%Temp is Row_Value-1,
	%single_row_move(ColuListTemp,Row_No,
%					Temp,Colu_Value,Blocks,
%					RowList).
	% \+ member(Row_No/Colu_No,RowList).

move(Single_Row,Row_No,
	[Row_Value|RRest],Colu_Value,Blocks,
	Plan):-

	length(RRest,RowLength),
	RowTemp is Row_No - RowLength,

	%length(Colu_Value,Len),
	%column(Len,ColuListTemp1),

	single_row_move(RowTemp,
					Row_Value,Colu_Value,Blocks,
					RowList),

	append(Single_Row, RowList, Plan),
	fitColu(Colu_Value,Plan).

solve_depthfirst(Node, Row_No,
				Row_Value,Colu_Value,Blocks,
				[Node|Path]):-
	depthfirst(Node, Row_No,
				Row_Value,Colu_Value,Blocks,
				Path).

depthfirst(Node,_,[],Colu_Value,_,[]):-
	goal(Node,Colu_Value,0).
	%length(Row_Value,Len),
	%Row_No =:= Len.

depthfirst(Node,Row_No,
			[Row_Value|RRest],Colu_Value,Blocks,
			[NextNode|Path]):-
	move(Node, Row_No,
		[Row_Value|RRest],Colu_Value,Blocks,
		NextNode),
	depthfirst(NextNode,Row_No,
				RRest,Colu_Value,Blocks,
				Path).

print_all([]).
print_all([X|Rest]) :- write(X), nl, print_all(Rest).

dupli(L1,N,L2) :- dupli(L1,N,L2,N).
dupli([],_,[],_).
dupli([_|Xs],N,Ys,0) :- dupli(Xs,N,Ys,N).
dupli([X|Xs],N,[X|Ys],K) :- K > 0, K1 is K - 1, dupli([X|Xs],N,Ys,K1).


init(C,R,Initlist):-
	dupli([' '],C,Col),
	dupli([Col],R,Initlist).

changeOut(Init,[],Init).

changeOut(Init,In,Out):-
	In=[H|T],
	H=R/C,
	nth1(R,Init,Row),
	replace(Row,C,'O',NewRow),
	replace(Init,R,NewRow,NewInit),
	changeOut(NewInit,T,Out).
changeOut2(Init,[],Init).

changeOut2(Init,In,Out):-
	In=[H|T],
	H=R/C,
	nth1(R,Init,Row),
	replace(Row,C,'X',NewRow),
	replace(Init,R,NewRow,NewInit),
	changeOut2(NewInit,T,Out).


replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).



solve(Row,Column,Blocks,Path):-
	check(Column,Row),
	length(Row,R),
	length(Column,C),
	solve_depthfirst([],R,Row,Column,Blocks,Path),
	last(Path,Solution),
	%print_all(Solution),
	init(C,R,Init),
	changeOut(Init,Solution,Out),
	changeOut2(Out,Blocks,Out2),
	print_all(Out2).
	% depthfirst([],1,Row_Value,Colu_Value,Blocks,Path).
