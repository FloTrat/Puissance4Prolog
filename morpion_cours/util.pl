%%%%%%%%%%%% util.pl %%%%%%%%%%%%
% Différents prédicats auxquels il faut accéder publiquement

:- module(util, [
	incr/2,
	decr/2,
	sum/2,
	caseVide/2
]).
%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% incr/2(+X, -X1)
% Unifie X1 = X+1.
% Vrai pour X1 = X+1.
incr(X,X1):-
	X1 is X+1.

% decr/2(+X, -X1)
% Unifie X1 = X-1.
% Vrai pour X1 = X-1.
decr(X,X1):-
	X1 is X-1.

% sum/2(+Liste, -Somme)
% Somme les termes de la liste.
% Somme s'unifie é la somme des termes de la liste.
sum([],0).
sum([X|Xs],N) :-
	sum(Xs,N1),
	N is N1+X.

% caseVide/2(+X, +Y)
% Vérifie si la case est vide.
% Vrai si la case n'a pas été remplie.
caseVide(X,Y) :-
	nonvar(X),
	nonvar(Y),
	not(case(X,Y,_)).
