%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


                                                      %%%%%%%%%%%%%%%%%%%%%%%%%
                                                      %%%%   DEPRECATED!   %%%%
                                                      %%%%%%%%%%%%%%%%%%%%%%%%%




% parcoursArbre/4(+J,+Pmax,-R,-Value)
% Parcours l'arbre de jeu en évaluant les feuilles grâces aux différentes fonctions d'évaluation. +J : joueur devant jouer, +Pmax : profondeur maximale, -R : le coup à jouer, -Value : évaluation du noeud courant.
parcoursArbre(J,Pmax,R,Value) :-
	initCaseTest,infinitePos(InfP),infiniteNeg(InfN),assert(maximizer(J)), assert(joueurCourant(J)),
	parcours(1,1,Pmax,[1,0],InfP,InfN), feuille([1,0],X1),
	setJoueur(1), parcours(2,1,Pmax,[2,0],InfP,X1), feuille([2,0],X2),
	setJoueur(1), AlphaNext is max(X1,X2), parcours(3,1,Pmax,[3,0],InfP,AlphaNext), feuille([3,0],X3),
	setJoueur(1), AlphaNext1 is max(AlphaNext,X3), parcours(4,1,Pmax,[4,0],InfP,AlphaNext1), feuille([4,0],X4),
	setJoueur(1), AlphaNext2 is max(AlphaNext1,X4), parcours(5,1,Pmax,[5,0],InfP,AlphaNext2), feuille([5,0],X5),
	setJoueur(1), AlphaNext3 is max(AlphaNext2,X5), parcours(6,1,Pmax,[6,0],InfP,AlphaNext3), feuille([6,0],X6),
	setJoueur(1), AlphaNext4 is max(AlphaNext3,X6), parcours(7,1,Pmax,[7,0],InfP,AlphaNext4), feuille([7,0],X7),
	coupAJouerMaximizer([X1,X2,X3,X4,X5,X6,X7],R,Value), clearTest,!. % the second call and the next ones are called with the result of the preceding (we take the max of all of them) on reset le joueur entre chaque call

parcoursArbre(J, Pmax, R, Value) :-
    idastar( 1, Solution ),
    idastar( 2, Solution ),
    idastar( 3, Solution ),
    idastar( 4, Solution ),
    idastar( 5, Solution ),
    idastar( 6, Solution ),
    idastar( 7, Solution ),
    coupAJouerMaximizer([X1,X2,X3,X4,X5,X6,X7],R,Value), clearTest,!. % the second call and the next ones are called with the result of the preceding (we take the max of all of them) on reset le joueur entre chaque call

%%%%%%%%%%%%%%%%%% ISBN : 978-0-321-41746-6 written by Ivan BRATKO %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% minimax copié collé %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
minimax( Pos, BestSucc, Val)  :-
  moves( Pos, PosList), !,               % Legal moves in Pos produce PosList
  best( PosList, BestSucc, Val)
   ;
   staticval( Pos, Val).                 % Pos has no successors: evaluate statically 

best( [ Pos], Pos, Val)  :-
  minimax( Pos, _, Val), !.

best( [Pos1 | PosList], BestPos, BestVal)  :-
  minimax( Pos1, _, Val1),
  best( PosList, Pos2, Val2),
  betterof( Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterof( Pos0, Val0, Pos1, Val1, Pos0, Val0)  :-        % Pos0 better than Pos1
  min_to_move( Pos0),                                    % MIN to move in Pos0
  Val0 > Val1, !                                         % MAX prefers the greater value
  ;
  max_to_move( Pos0),                                    % MAX to move in Pos0
  Val0 < Val1, !.                                % MIN prefers the lesser value 

betterof( Pos0, Val0, Pos1, Val1, Pos1, Val1).           % Otherwise Pos1 better than Pos0

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% IDA* copié collé %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% idastar( Start, Solution):
%     Perform IDA* search; Start is the start node, Solution is the solution path
idastar( Start, Solution) :-
  retract( next_bound(_)), fail    % Clear next_bound
  ;
  asserta( next_bound( 0)),        % Initialize bound
  idastar0( Start, Solution).

idastar0( Start, Sol) :-
  retract( next_bound( Bound)),    % Current bound
  asserta( next_bound( 99999)),    % Initialize next bound
  f( Start, F),                    % f-value of start node
  df( [Start], F, Bound, Sol)      % Find solution; if not, change bound
  ;
  next_bound( NextBound),
  NextBound < 99999,               % Bound finite
  idastar0( Start, Sol).           % Try with new bound

% df( Path, F, Bound, Sol):
% Perform depth-first search within Bound
% Path is the path from start node so far (in reverse order)
% F is the f-value of the current node, i.e. the head of Path
df( [N|Ns], F, Bound, [N|Ns] ) :-
	F =< Bound,
	goal(N).                         % Succeed : solution found.

df( [N|Ns], F, Bound, Sol ) :-
	F =< Bound,                      % Node N within f-bound
	s(N, N1), \+ member( N1, Ns ),   % Expand N
	f( N1, F1 ),
	df( [N1, N|Ns], F1, Bound, Sol ).

df( _, f, Bound, _ ) :-
	F > Bound,                       % Beyond Bound
	update_next_bound(F),            % Just update next bound
	fail.                            % and fail

update_next_bound(F) :-
	next_bound( Bound ),
	Bound =< F, !                    % Do not change next bound
	;
	retract( next_bound(Bound) ), !, % Lower next bound
	asserta( next_bound(F) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%