% 201560012,20156014,20156005,20156023
word(zombifies, [z,o,m,b,i,f,i,e,s]).
word(akecabele, [a,k,e,c,a,b,e,l,e]). 
word(brickwork, [b,r,i,c,k,w,o,r,k]). 
word(backcheck, [b,a,c,k,c,h,e,c,k]). 
word(acmrremad,[a,c,m,r,r,e,m,a,d]). 
word(nhgwpfabz,[n,h,g,w,p,f,a,b,z]). 
word(jellybean,    [j,e,l,l,y,b,e,a,n]). 
word(earreoded,   [e,a,r,r,e,o,d,e,d]). 

%asserta saves the word as a fact.
%atom_chars turns string to a list of chars.

readWords():-
write('please type word1: '),
nl,
read(X1),
atom_chars(X1,Y1),
asserta(word(X1,Y1)),
write('please type word2: '),
nl,
read(X2),
atom_chars(X2,Y2),
asserta(word(X2,Y2)),
write('please type word3: '),
nl,
read(X3),
atom_chars(X3,Y3),
asserta(word(X3,Y3)),
write('please type word4: '),
nl,
read(X4),
atom_chars(X4,Y4),
asserta(word(X4,Y4)),
write('please type word5: '),
nl,
read(X5),
atom_chars(X5,Y5),
asserta(word(X5,Y5)),
write('please type word6: '),
nl,
read(X6),
atom_chars(X6,Y6),
asserta(word(X6,Y6)),
write('please type word7: '),
nl,
read(X7),
atom_chars(X7,Y7),
asserta(word(X7,Y7)),
write('please type word8: '),
nl,
read(X8),
atom_chars(X8,Y8),
asserta(word(X8,Y8)).


/*
%checks if all the words are different and not equal.
diffVals([H1,H2,H3,H4,V1,V2,V3,V4]):-
H1\=V1, H1\=V2 , H1\=V3 ,H1\=V4 , H2\=V1 , H2\=V2 ,H2\=V3 ,H2\=V4,
H3\=V1 , H3\=V2 ,H3\=V3 , H3\=V4 , H4\=V1 , H4\=V2 , H4\=V3 ,H4\=V4 ,
H1\=H2 , H2\=H3 ,H3\=H4 ,H2\=H4 ,H1\=H4 ,V1\=V2 ,V2\=V3 ,V3\=V4 ,V2\=V4 ,V1\=V4.
*/

%State contains [8lists(character array)].
is_goal([H1,H2,H3,H4,V1,V2,V3,V4]):-
V4\=0,V3\=0, V2\=0, V1\=0, H4\=0, H1\=0 , H2\=0 , H3\=0.


%add a word to H1 , var/1 returns true if the given variable doesnt have a value yet (is empty).
move([H1,H2,H3,H4,V1,V2,V3,V4],[NH1,H2,H3,H4,V1,V2,V3,V4]):-
   H1=0,
     H2=0,H3=0,H4=0,V1=0,V2=0,V3=0,V4=0 ,
   word(NH1, [_, _, _, _, _, _, _, _, _]).
 
   


%add a word to H2.
move([H1,H2,H3,H4,V1,V2,V3,V4],[H1,NH2,H3,H4,V1,V2,V3,V4]):-
   H2=0, H1\=0,
   H3=0,H4=0,V1=0,V2=0,V3=0,V4=0,
   word(NH2, [_, _, _, _, _, _, _, _, _]),
   NH2\=H1.
  
   


%add a word to H3.
move([H1,H2,H3,H4,V1,V2,V3,V4],[H1,H2,NH3,H4,V1,V2,V3,V4]):-
   H3=0, H1\=0 , H2\=0,
   H4=0,V1=0,V2=0,V3=0,V4=0,
   word(NH3, [_, _, _, _, _, _, _, _, _]),
   NH3\=H2,NH3\=H1.
   


%add a word to H4.
move([H1,H2,H3,H4,V1,V2,V3,V4],[H1,H2,H3,NH4,V1,V2,V3,V4]):-
   H4=0, H1\=0 , H2\=0 , H3\=0,
   V1=0,V2=0,V3=0,V4=0,
   word(NH4, [_, _, _, _, _, _, _, _, _]),
   NH4\=H3,NH4\=H2,NH4\=H1.


%add a word to V1.
move([H1,H2,H3,H4,V1,V2,V3,V4],[H1,H2,H3,H4,NV1,V2,V3,V4]):-
   V1=0, H4\=0, H1\=0 , H2\=0 , H3\=0,
   V2=0,V3=0,V4=0,

   word(NV1, [V11, V12, V13, V14, V15, V16, V17, V18, V19]),
   word(H1, [H11, V12, H13, V22, H15, V32, H17, V42, H19]),
   word(H2, [H21, V14, H23, V24, H25, V34, H27, V44, H29]),
   word(H3, [H31, V16, H33, V26, H35, V36, H37, V46, H39]),
   word(H4, [H41, V18, H43, V28, H45, V38, H47, V48, H49]).
   NV1\=H1, NV1\=H2 , NV1\=H3 ,NV1\=H4.

%add a word to V2.
move([H1,H2,H3,H4,V1,V2,V3,V4],[H1,H2,H3,H4,V1,NV2,V3,V4]):-
   V2=0, V1\=0, H4\=0, H1\=0 , H2\=0 , H3\=0,
   V3=0,V4=0,
   word(NV2, [V21, V22, V23, V24, V25, V26, V27, V28, V29]),
   word(H1, [H11, V12, H13, V22, H15, V32, H17, V42, H19]),
   word(H2, [H21, V14, H23, V24, H25, V34, H27, V44, H29]),
   word(H3, [H31, V16, H33, V26, H35, V36, H37, V46, H39]),
   word(H4, [H41, V18, H43, V28, H45, V38, H47, V48, H49]).
   NV2\=H1, NV2\=H2 , NV2\=H3 ,NV2\=H4 , NV2\=V1.


%add a word to V3.
move([H1,H2,H3,H4,V1,V2,V3,V4],[H1,H2,H3,H4,V1,V2,NV3,V4]):-
   V3=0, V2\=0, V1\=0, H4\=0, H1\=0 , H2\=0 , H3\=0,
   V4=0,
   word(NV3, [V31, V32, V33, V34, V35, V36, V37, V38, V39]),
   word(H1, [H11, V12, H13, V22, H15, V32, H17, V42, H19]),
   word(H2, [H21, V14, H23, V24, H25, V34, H27, V44, H29]),
   word(H3, [H31, V16, H33, V26, H35, V36, H37, V46, H39]),
   word(H4, [H41, V18, H43, V28, H45, V38, H47, V48, H49]).
   NV3\=H1, NV3\=H2 , NV3\=H3 ,NV3\=H4 , NV3\=V1 , NV3\=V2 .


%add a word to V4.
move([H1,H2,H3,H4,V1,V2,V3,V4],[H1,H2,H3,H4,V1,V2,V3,NV4]):-
    V4=0,V3\=0, V2\=0, V1\=0, H4\=0, H1\=0 , H2\=0 , H3\=0,
	word(NV4, [V41, V42, V43, V44, V45, V46, V47, V48, V49]),
    word(H1, [H11, V12, H13, V22, H15, V32, H17, V42, H19]),
    word(H2, [H21, V14, H23, V24, H25, V34, H27, V44, H29]),
    word(H3, [H31, V16, H33, V26, H35, V36, H37, V46, H39]),
    word(H4, [H41, V18, H43, V28, H45, V38, H47, V48, H49]).
	NV4\=H1, NV4\=H2 , NV4\=H3 ,NV4\=H4 , NV4\=V2 , NV4\=V3 , NV4\=V1.


	printSolution([H1,H2,H3,H4,V1,V2,V3,V4]):-
	   write('H1:'),
	   write(H1),nl,
	   write('H2:'),
	   write(H2),nl,
	   write('H3:'),
	   write(H3),nl,
	   write('H4:'),
	   write(H4),nl,
	   write('V1:'),
	   write(V1),nl,
	   write('V2:'),
	   write(V2),nl,
	   write('V3:'),
	   write(V3),nl,
	   write('V4:'),
	   write(V4),nl.
	   
	
go():-
   path( [ [ [0,0,0,0,0,0,0,0] ,null] ] , [] ).
	

moves( Child, Open, Closed,[Next,Child]):-

	move(Child,Next),
	\+ member([Next,_],Open),
	\+ member([Next,_],Closed).



% if open is empty then there is no solution.
path([],_):-
	write('No solution'),nl,!.
	
path([[Goal,Parent] | RestOfOpen], Closed):-
    is_goal(Goal),!,
	write('A solution is found'), nl,
	write(Goal),!.
	
	
path(Open, Closed):-
	removeFromOpen(Open, [Child, Parent], RestOfOpen),
	%write([Child,Parent]),nl,
	getchildren(Child, Open, Closed, Children),
	%write(Children),nl,
	append(Children ,RestOfOpen, NewOpen),
	%write('RestOfOpen'),nl,
	%write(RestOfOpen),nl,
	path(NewOpen, [[Child, Parent] | Closed]).

	
removeFromOpen([State|RestOpen], State, RestOpen).
	
%Depth-first search.	
addListToOpen([],Open,Open).
addListToOpen(L,Open,[L|Open]).



getchildren(Child, Open ,Closed , Children):-
	bagof(X, moves( Child, Open, Closed, X), Children), ! .

	
	
%return an empty list if there are no children
getchildren(_,_ ,_ , []).























