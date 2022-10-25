%%%%%%%%%%%% eval.pl %%%%%%%%%%%%
% Différentes fonctions d'évaluation pour le Puissance 4, toutes basées sur des heuristiques différentes.

:- module(eval, [evalJeu/5, evalTest1/2]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).
:- use_module(jeu).
:- use_module(ia).
:- use_module(miniMax).

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% evalJeu/5(+JoueurCourant, +AutreJoueur, +X, +Y, -Score)
% Évalue la situation courante pour le joueur JoueurCourant étant donné que le dernier coup joué fut joué en (X,Y). Le score est pondéré par les différentes pondérations données en entrée (par assert) à evalJeu. Le score est ensuite perturbé par une valeur aléatoire, permettant de casser le caractère déterministe de l'IA.
% Score s'unifie avec le score évalué pour la position courante.
evalJeu(JoueurCourant,AutreJoueur,X,Y,Score) :-
	assert(caseTest(X,Y,JoueurCourant)),
	assert(ennemiTest(AutreJoueur)),
	poidsPuissance3(PoidsPuissance3), poidsPosition(PoidsPosition), poidsDensite(PoidsDensite), poidsAdjacence(PoidsAdjacence),
	evalPosition(JoueurCourant,Score1,PoidsPosition),
	evalPuissances3(JoueurCourant,AutreJoueur,Score2,PoidsPuissance3),
	densite(JoueurCourant,Score3,PoidsDensite),
	evalAdjacence(X,Y,_,Score4, PoidsAdjacence),
	retract(caseTest(X,Y,JoueurCourant)),
	retract(ennemiTest(AutreJoueur)),
	random_between(-2,2,Perturbation),
	Score is Score1 * PoidsPosition
			+ Score2 * PoidsPuissance3
			+ Score3
			+ Score4
			+ Perturbation.

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% evalPosition/3(+Courant,-Score,+PoidsPosition)
% Évalue en privilégiant les positions centrales en fonction de la pondération.
% Score s'unifie à une valeur entre -400 et 400.
evalPosition(Courant,Score,PoidsPosition) :-
	PoidsPosition>0,
	assert(nbCasesPleines(0)),
	findall(S, evalCases(Courant,S), Scores),
	sum(Scores, ScoreTot),
	nbCasesPleines(NbCasesPleinesFinal),
	retract(nbCasesPleines(NbCasesPleinesFinal)),
	Score is ScoreTot / (NbCasesPleinesFinal+1).
evalPosition(_,0,_).

evalCases(Courant,ScoreCase) :-
	caseTest(X,Y,_),
	nbCasesPleines(NbCasesPleines),
	retract(nbCasesPleines(NbCasesPleines)),
	incr(NbCasesPleines,NbCasesPleinesF),
	assert(nbCasesPleines(NbCasesPleinesF)),
	evalCase(X,Y,Courant,ScoreCase).

% renvoie un score entre -400 et 400
evalCase(X,Y,Courant,ScoreCase) :-
	nbColonnes(NBCOLONNES),
	nbLignes(NBLIGNES),
	ponderationJ(X, Y, Courant, PonderationJoueur),
	CentreX is NBCOLONNES // 2 + 1,
	CentreY is NBLIGNES // 2 + 1,
	Dx is X - CentreX,
	Dy is Y - CentreY,
	abs(Dx,AbsX),
	abs(Dy,AbsY),
	ScoreCase is ( 200/(AbsX+1) + 200/(AbsY+1) )*PonderationJoueur.

ponderationJ(X,Y, Courant,1) :-
	caseTest(X,Y,Courant), !.
ponderationJ(X,Y,_,-1) :-
	ennemiTest(J),
	caseTest(X,Y,J), !.
ponderationJ(_,_,_,0).

%%%%%%%%%%%%%%%%%%%%

% evalPuissances3/3(+JoueurCourant,+AutreJoueur,-Score)
% Évalue en cherchant les positions faisant gagner.
% ScoreFinal s'unifie au score de la position.
evalPuissances3(JoueurCourant,AutreJoueur,ScoreFinal,PoidsPuissance3) :-
	PoidsPuissance3>0,
	findall(S,evalCasesVides(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalCasesVides(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant - ScoreAutre.
evalPuissances3(_,_,0,_).

evalCasesVides(Joueur,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseTest(X,Y,Joueur),
	incr(X,X1),
	decr(X,X2),
	incr(Y,Y1),
	decr(Y,Y2),
	caseVideTest(X1,Y1),
	caseVideTest(X2,Y1),
	caseTest(X2,Y2,_),
	caseTest(X1,Y2,_),
	(gagneTestDirect(X1,Y1,Joueur) -> ScoreCase1=100 ; ScoreCase1=0),
	(gagneTestDirect(X2,Y1,Joueur) -> ScoreCase2=100 ; ScoreCase2=0),
	ScoreCase is ScoreCase1+ScoreCase2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR ADJACENCE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evalAdjacence/5(+X,+Y,+Joueur,-Note,+PoidsAdjacence)
% Donne une note d'autant plus forte qu'un pion est entouré de pions amis.
% Note s'unifie au score de la position.

evalAdjacence(X,Y,Joueur,Note,PoidsAdjacence) :-
	PoidsAdjacence>0,
	aggregate_all(count,caseAdjacente(X,Y,Joueur,_,_),N),
	pow(N,2,Note).
evalAdjacence(_,_,_,0,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR DENSITE DE PION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% densite/3(+Joueur,-Note,+PoidsDensite)
% Donne une note d'autant plus élevée que les pions sont groupés.
% Note s'unifie au score de la position.
densite(J,Note,PoidsDensite) :- PoidsDensite>0, Z is 1, calculNbPoints(J,Z,Note).
densite(_,0,_).
calculNbPoints(_,Z,Note) :- Z>6, Note is 0.
calculNbPoints(J,Z,Note) :- nbPointsZone(J,Z,N), incr(Z,ZP), calculNbPoints(J,ZP,NP), Note is N+NP.
nbPointsZone(J,Z,NbPoints) :- nbPionsZone(J,Z,N), pow(N,2,NbPoints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nbPionsZone/3(+Joueur,+Zone,-NbPions)
% Donne le nombre de pions contenu dans une zone.
% NbPions s'unifie au nombre de pions contenu dans une zone.
nbPionsZone(J,Z,NbPions) :-
	aggregate_all(count,caseTestZone(Z,J,_,_),NbPions).

caseTestZone(Zone,Joueur,X,Y) :- caseTest(X,Y,Joueur), zone(Zone,X,Y).
zone(1,X,Y) :- X =<3, Y =< 3.
zone(2,X,Y) :- X = 4, Y =< 3.
zone(3,X,Y) :- X > 4, Y =< 3.
zone(4,X,Y) :- X > 4, Y > 3.
zone(5,X,Y) :- X = 4, Y > 3.
zone(6,X,Y) :- X =<3, Y > 3.



%%%%% gagneTestDirect %%%%%


gagneTestDirect(X,Y,J) :-
	gagneTestDirectLigne(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag1(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag2(X,Y,J).


%%% En ligne %%%

gagneTestDirectLigne(X,Y,J) :-
	decr(X,X1),
	gaucheVerif(X1,Y,J,Rg),
	incr(X,_),
	droiteVerif(X,_,J,Rd),
	!,
	Rf is Rg+Rd, Rf>2.

gaucheVerif(X,Y,J,Rg):-
	gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gauche(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):-
	droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droite(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneTestDirectDiag1(X,Y,J) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheHautVerif(X1,Y1,J,Rg),
	incr(X,X2),
	decr(Y,Y2),
	droiteBasVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheHautVerif(X,Y,J,Rg):-
	gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):-
	droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneTestDirectDiag2(X,Y,J) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheBasVerif(X1,Y1,J,Rg),
	incr(X,X2),
	incr(Y,Y2),
	droiteHautVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheBasVerif(X,Y,J,Rg) :-
	gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg) :-
	droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg).

%%%%%%% caseVideTest %%%%%
% caseVideTest(+X,+Y)
% vrai si la case X,Y est vide
caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).


%%%% Utilisé pour les tests unitaires

evalTest1(1,-3).
evalTest1(2,-4).
evalTest1(3,5).
evalTest1(4,10).
evalTest1(5,9).
evalTest1(6,-5).
evalTest1(7,8).
