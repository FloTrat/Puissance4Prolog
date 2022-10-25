%%%%%%%%%%%% miniMax.pl %%%%%%%%%%%%
% Implémentation de minimax avec diverses optimisations propres au Puissance 4.

:- module(miniMax, [parcoursArbre/4, caseTest/3, gagneTest/4]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).
:- use_module(eval).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic caseTest/3.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

initCaseTest :- case(X,Y,Z), assert(caseTest(X,Y,Z)), false. %on assert une caseTest pour toutes les cases.
initCaseTest.

clearTest :-
	retractall(caseTest(X,Y,_)),
	retractall(feuille(X,Y)),
	retract(maximizer(X)), retract(joueurCourant(_)). % on eve tout ce que l'on a ajouté.

parcours(X, _, _, L, _, _) :-
	nbLignes(MaxLignes),jeu:case(X,MaxLignes,_), joueurCourant(Joue), maximizer(Joue), infiniteNeg(2,Value), assert(feuille(L, Value)). % on ne peut PAS jouer, on met -infini
parcours(X, _, _, L, _, _):-
	nbLignes(MaxLignes),jeu:case(X,MaxLignes,_), joueurCourant(Joue), not(maximizer(Joue)), infinitePos(2,Value), assert(feuille(L, Value)). % on ne peut PAS jouer, on met +infini
parcours(X, _, _, L, _, _):-
	nbLignes(MaxLignes),caseTest(X,MaxLignes,_), joueurCourant(Joue), evaluate(X,MaxLignes,Joue,Value), assert(feuille(L, Value)) .% on ne peut plus jouer, on met une feuille (on évalue)
parcours(X, P, _, L, _, _):-
	joueurCourant(Joue), calculPositionJeton(X, 1, Y), gagneTest(X,Y,Joue,Direct), victoireDirecte(X,Y,Joue,L,P,Direct).

parcours(X, P, Pmax, L, _, _):- P==Pmax,joueurCourant(Joue), placerJeton(X,Y,Joue), evaluate(X, Y, Joue, Value),assert(feuille(L, Value)),retract(caseTest(X,Y,Joue)). % on est à la prof max, on evalue et on met une feuille
parcours(X, P, Pmax, L, Beta, Alpha) :- incr(P, P1),joueurCourant(Joue), placerJeton(X,Y,Joue), %on incremente la profondeur, puis on joue un coup(qui réussit a tous les coups)
	setJoueur(P1), %on set le joueur
	attribueVal(ValeurPrec), % on initialise val
	parcours(1, P1,Pmax, [1|L], Beta, Alpha), %on joue colonne 1
	feuille([1|L], Valeur1),%here is the value of first branch

	setJoueur(P1), % on reset le joueur (il a changé dans le premier parcours)
	choixVal(Valeur1,ValeurPrec,Val1),%choisit si min ou max, renvoie la valeur pour le prochain coup.

	joueCoupSuivant(Val1,2,P1,Pmax,L,Beta,Alpha,Val2,Beta2,Alpha2),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), % on reset le joueur (il a changé dans le premier parcours)

	joueCoupSuivant(Val2,3,P1,Pmax,L,Beta2,Alpha2,Val3,Beta3,Alpha3),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	joueCoupSuivant(Val3,4,P1,Pmax,L,Beta3,Alpha3,Val4,Beta4,Alpha4),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	joueCoupSuivant(Val4,5,P1,Pmax,L,Beta4,Alpha4,Val5,Beta5,Alpha5),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	joueCoupSuivant(Val5,6,P1,Pmax,L,Beta5,Alpha5,Val6,Beta6,Alpha6),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	joueCoupSuivant(Val6,7,P1,Pmax,L,Beta6,Alpha6,Valeur,_,_),%on tente le coup suivant (ou pas si élagage), avec la valeur retournée par le précédent
	setJoueur(P1), %on change de joueur

	retract(caseTest(X,Y,Joue)), %on annule le coup pour poursuivre dans l'arbre
	feuille([1|L], _),feuille([2|L], _), %on cherche les feuilles associées (elles ont été calculées plus bas dans l'arbre)
	setJoueur(P1), %on change de joueur
	assert(feuille(L,Valeur)),joueurCourant(_). %on met notre feuille calculée


victoireDirecte(_,_,J,L,P,1):- maximizer(J), Pp is 1-P, infinitePos(Pp,Value), assert(feuille(L, Value)). %Victoire du max
victoireDirecte(_,_,J,L,P,1):- not(maximizer(J)), Pp is 1-P, infiniteNeg(Pp,Value), assert(feuille(L, Value)). %Victoire du min

victoireDirecte(X,Y,J,_,_,0):- assert(caseTest(X,Y,J)), false.

victoireDirecte(X,Y,J,L,P,0):- maximizer(J), Pp is -P, infinitePos(Pp,Value),
	autreJoueur(J2), testDefaiteProchaine(J2),
	assert(feuille(L, Value)), retract(caseTest(X,Y,J)). %Victoire anticipée du max
victoireDirecte(X,Y,J,L,P,0):- not(maximizer(J)), Pp is -P, infiniteNeg(Pp,Value),
	autreJoueur(J2), testDefaiteProchaine(J2),
	assert(feuille(L, Value)), retract(caseTest(X,Y,J)). %Victoire anticipée du min

victoireDirecte(X,Y,J,_,_,0):- retract(caseTest(X,Y,J)), false. %ménage si on perde derrière

victoireDirecte(X,Y,J,_,_,-5):- assert(caseTest(X,Y,J)), false.

victoireDirecte(X,Y,J,L,P,-5):- maximizer(J), Pp is -5-P, infinitePos(Pp,Value),
	autreJoueur(J2), testDefaiteAnticipeeProchaine(J2),
	assert(feuille(L, Value)), retract(caseTest(X,Y,J)). %Victoire anticipée du max
victoireDirecte(X,Y,J,L,P,-5):- not(maximizer(J)), Pp is -5-P, infiniteNeg(Pp,Value),
	autreJoueur(J2), testDefaiteAnticipeeProchaine(J2),
	assert(feuille(L, Value)), retract(caseTest(X,Y,J)). %Victoire anticipée du min

victoireDirecte(X,Y,J,_,_,-5):- retract(caseTest(X,Y,J)), false. %ménage si on perde derrière

testDefaiteProchaine(J):-
	calculPositionJeton(1,1,Y1), not(gagneTestDirect(1,Y1,J)),
	calculPositionJeton(2,1,Y2), not(gagneTestDirect(2,Y2,J)),
	calculPositionJeton(3,1,Y3), not(gagneTestDirect(3,Y3,J)),
	calculPositionJeton(4,1,Y4), not(gagneTestDirect(4,Y4,J)),
	calculPositionJeton(5,1,Y5), not(gagneTestDirect(5,Y5,J)),
	calculPositionJeton(6,1,Y6), not(gagneTestDirect(6,Y6,J)),
	calculPositionJeton(7,1,Y7), not(gagneTestDirect(7,Y7,J)).

testDefaiteAnticipeeProchaine(J):-
	calculPositionJeton(1,1,Y1), not((gagneTest(1,Y1,J,V1), V1>=0)),
	calculPositionJeton(2,1,Y2), not((gagneTest(2,Y2,J,V2), V2>=0)),
	calculPositionJeton(3,1,Y3), not((gagneTest(3,Y3,J,V3), V3>=0)),
	calculPositionJeton(4,1,Y4), not((gagneTest(4,Y4,J,V4), V4>=0)),
	calculPositionJeton(5,1,Y5), not((gagneTest(5,Y5,J,V5), V5>=0)),
	calculPositionJeton(6,1,Y6), not((gagneTest(6,Y6,J,V6), V6>=0)),
	calculPositionJeton(7,1,Y7), not((gagneTest(7,Y7,J,V7), V7>=0)).

% evaluate/4(+X, +Y, +Joueur, -Score)
% Évalue la position en fonction des pondérations passées en prédicat à l'IA.
% Score s'unifie au score de la position.
evaluate(X,Y,Joueur,Score) :-
	ennemi(Joueur,AutreJoueur),
	evalJeu(Joueur,AutreJoueur,X,Y,Score1),
	minOuMax(Joueur,Score1,Score).

minOuMax(Joueur,Score,-Score):- %minimizer
	not(maximizer(Joueur)).
minOuMax(_,Score,Score). %maximizer


%%%%%%%%%%%%%%%%%%%%%%%%
%% Élagage alpha-bêta %%
%%%%%%%%%%%%%%%%%%%%%%%%


choixVal(Valeur1,ValeurPrec,Val1):- joueurCourant(Joue), maximizer(Joue), Val1 is max(Valeur1, ValeurPrec). % we choose after the first choice if we take max or min for val
choixVal(Valeur1,ValeurPrec,Val1):- Val1 is min(Valeur1, ValeurPrec).

attribueVal(X):- infiniteNeg(InfN), joueurCourant(Joue), maximizer(Joue), X is InfN. % the initial value of a node (-inf if maximizer, +inf if minimizer)
attribueVal(X):-infinitePos(InfP), X is InfP.


%%%For the Minimizer
joueCoupSuivant(ValeurPrec,ColonneAJouer,_,_,L,Beta,Alpha,Val,Beta,Alpha):-joueurCourant(Joue), not(maximizer(Joue)), ValeurPrec =< Alpha, Val is ValeurPrec, assert(feuille([ColonneAJouer|L], Val)).%coupure alpha !!
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L,Beta,Alpha,Val,BetaCalc,Alpha):-joueurCourant(Joue), not(maximizer(Joue)), BetaCalc is min(Beta, ValeurPrec), parcours(ColonneAJouer, P1,Pmax,[ColonneAJouer|L],BetaCalc, Alpha), feuille([ColonneAJouer|L], ValeurFils), Val is min(ValeurFils, ValeurPrec). %pas de coupure!


%%For the Maximizer
joueCoupSuivant(ValeurPrec,ColonneAJouer,_,_,L, Beta, Alpha,Val,Beta,Alpha):-joueurCourant(Joue), maximizer(Joue), ValeurPrec >= Beta, Val is ValeurPrec,assert(feuille([ColonneAJouer|L], Val)).%coupure beta !!
joueCoupSuivant(ValeurPrec,ColonneAJouer,P1,Pmax,L, Beta, Alpha,Val,Beta,AlphaCalc):-joueurCourant(Joue), maximizer(Joue), AlphaCalc is max(Alpha, ValeurPrec), parcours(ColonneAJouer, P1,Pmax,[ColonneAJouer|L],Beta, AlphaCalc),feuille([ColonneAJouer|L], ValeurFils),Val is max(ValeurFils, ValeurPrec). %pas de coupure!





%%%%%%%%%% Aide calcul. Redundant code, do not remove %%%%%%%%%%%%%
setJoueur(P):- parite(P), maximizer(jaune), joueurCourant(jaune),retract(joueurCourant(_)), assert(joueurCourant(rouge)),!. % si P pair, alors c'est au minimizer de jouer
setJoueur(P):- parite(P), maximizer(rouge), joueurCourant(rouge),retract(joueurCourant(_)), assert(joueurCourant(jaune)),!.
setJoueur(P):- not(parite(P)),maximizer(rouge), joueurCourant(jaune),retract(joueurCourant(_)), assert(joueurCourant(rouge)),!. % P impair, maximizer joue
setJoueur(P):- not(parite(P)),maximizer(jaune), joueurCourant(rouge),retract(joueurCourant(_)), assert(joueurCourant(jaune)).
setJoueur(_).

choixValeurNoeud(L,R,Value):- joueurCourant(X), maximizer(X), coupAJouerMaximizer(L,R,Value),!. %on choisit val min ou max
choixValeurNoeud(L,R,Value):- coupAJouerMinimizer(L,R,Value).


coupAJouerMaximizer(L, R,X):- membreMaxRank(X,L,R).
coupAJouerMinimizer(L, R,X):- membreMinRank(X,L,R).


parite(X):- divmod(X ,2, _, 0).

changerJoueur:- joueurCourant(jaune), retract(joueurCourant(_)), assert(joueurCourant(rouge)).
changerJoueur:- joueurCourant(rouge), retract(joueurCourant(_)), assert(joueurCourant(jaune)).

autreJoueur(rouge):- joueurCourant(jaune).
autreJoueur(jaune):- joueurCourant(rouge).

membreMaxRank(X, [Y|L], R):- membreMax(L, 1, 1, R, Y, X),!.
membreMax([], _, Rm, Rm, Max, Max).
membreMax([Y|L], R1, Rm, R, Max, X):- incr(R1,R2), maximum(Y, Max, Rm, R2, NewMax, NewRankMax), membreMax(L,R2,NewRankMax,R, NewMax, X).

membreMinRank(X, [Y|L], R):- membreMin(L, 1, 1, R, Y, X),!.
membreMin([], _, Rm, Rm, Max, Max).
membreMin([Y|L], R1, Rm, R, Max, X):- incr(R1,R2), minimum(Y, Max, Rm, R2, NewMax, NewRankMax), membreMin(L,R2,NewRankMax,R, NewMax, X).

minimum(Y, Max, _, NewRankMax, Y, NewRankMax):- Y<Max.
minimum(_, Max,OldRankMax, _, Max, OldRankMax).

maximum(Y, Max, _, NewRankMax, Y, NewRankMax):- Y>Max.
maximum(_, Max,OldRankMax, _, Max, OldRankMax).



%%% Place un jeton

% placerJeton/3(-Colonne, +Ligne, -Couleur)
% Insère si possible un jeton dans la colonne donnée.
% Y s'unifie à la ligne d'insertion ou échoue.
placerJeton(X,Y,C) :- coupValide(X), insererJeton(X, Y, C).

%%%%% placerJeton %%%%%
% coupValide/1(-Colonne)
% Vérifie si un jeton est jouable dans cette colonne.
% Vrai si le coup est valide.
coupValide(X) :- nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1, nbLignes(NBLIGNES), caseVideTest(X,NBLIGNES).

% insererJeton/3(-Colonne, +Ligne, -Couleur)
% Insère, sans vérification, un jeton de la couleur donnée, dans la colonne donnée.
% Y s'unifie à la ligne d'insertion,
insererJeton(X,Y,C) :- calculPositionJeton(X, 1, Y), assert(caseTest(X,Y,C)).

% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% Calcule la première ligne vide d'une colonne.
% Ligne s'unfinie à l'indice de la première ligne vide de la colonne.
calculPositionJeton(X,YCheck,YCheck) :- caseVideTest(X,YCheck), !.
calculPositionJeton(X,YCheck,Y) :- incr(YCheck, YCheck1), calculPositionJeton(X,YCheck1,Y).

%%% Détection de la victoire des cases de test.

gagneTest(X,Y,J,V) :- %V=1 si victoire direct, 0 si indirect
	assert(caseTest(X,Y,J)),
	gagneColonneTest(X,Y,J,R1,A1),
	gagneLigneTest(X,Y,J,R2,P2,A2),
	gagneDiag1Test(X,Y,J,R3,P3,A3),
	gagneDiag2Test(X,Y,J,R4,P4,A4),
	Pf is P2+P3+P4,
	Af is A1+A2+A3+A4,
	testFinal(R1,R2,R3,R4,Pf,Af,V),
	retract(caseTest(X,Y,J)).

gagneTest(X,Y,J,0):-retract(caseTest(X,Y,J)), false. %ménage

testPotentielAccumulation(X,Y,J,P,A):-
	testPotentiel(X,Y,J,P), %Peut on la remplir au prochain coup?
	testAccumulation(X,Y,J,A). %As-t-on accumulation?

testPotentiel(_,1,_,1).	%case au niveau 1
testPotentiel(X,Y,_,1):-
	decr(Y,Y1),
	caseTest(X,Y1,_).  %On peut la remplir
testPotentiel(_,_,_,0). %On ne peut pas la remplir


testAccumulation(X,Y,J,1) :- incr(Y,Y1), caseTestValideVide(X,Y1), gagneTestDirect(X,Y1,J). %Case au dessus gagnante aussi
testAccumulation(X,Y,J,1) :- decr(Y,Y1), caseTestValideVide(X,Y1), gagneTestDirect(X,Y1,J). %Case en dessous gagnante aussi
testAccumulation(_,_,_,0). %Pas d'accumulation.

caseTestValideVide(X,Y):-
	nbColonnes(NBCOLONNES), X=<NBCOLONNES, X>=1,
	caseVideTest(X,Y). %Case vide

testFinal(R1,_,_,_,_,_,1):-
	R1 > 2.
testFinal(_,R2,_,_,_,_,1):-
	R2 > 2.
testFinal(_,_,R3,_,_,_,1):-
	R3 > 2.
testFinal(_,_,_,R4,_,_,1):-
	R4 > 2.
testFinal(_,_,_,_,P,_,0):-
	P>1.
testFinal(_,_,_,_,_,A,-5):-
	A >0.

%%%%% gagne %%%%%


%%% En colonne %%%

gagneColonneTest(X,Y,J,3,0) :-
	decr(Y,Y1),
	caseTest(X,Y1,J),
	decr(Y1,Y2),
	caseTest(X,Y2,J),
	decr(Y2,Y3),
	caseTest(X,Y3,J). %ligne en bas
gagneColonneTest(X,Y,J,0,1) :-
	decr(Y,Y1),
	caseTest(X,Y1,J),
	decr(Y1,Y2),
	caseTest(X,Y2,J),
	incr(Y,Ytemp),
	incr(Ytemp,Ydessus),
	gagneTestDirect(X,Ydessus,J).
gagneColonneTest(_,_,_,0,0).

%%% En ligne %%%

gagneLigneTest(X,Y,J,Rf,Pf,Af) :-
	decr(X,X1),
	gaucheTestVerif(X1,Y,J,Rg,Pg,Ag),
	incr(X,X2),
	droiteTestVerif(X2,Y,J,Rd,Pd,Ad),
	!,
	Rf is Rg+Rd, Pf is Pg+Pd, Af is Ag+Ad.

gaucheTestVerif(X,Y,J,Rg,Pg,Ag):-
	gaucheTest(X,Y,J,0,Rg,Pg,Ag).
gaucheTest(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectLigne(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
gaucheTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gaucheTest(X,Y,J,R,Rg,Pg,Ag) :-
	decr(X,X1),
	incr(R,R1),
	gaucheTest(X1,Y,J,R1,Rg,Pg,Ag).

droiteTestVerif(X,Y,J,Rg,Pg,Ag):-
	droiteTest(X,Y,J,0,Rg,Pg,Ag).
droiteTest(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectLigne(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
droiteTest(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droiteTest(X,Y,J,R,Rg,Pg,Ag) :-
	incr(X,X1),
	incr(R,R1),
	droiteTest(X1,Y,J,R1,Rg,Pg,Ag).

%%% En diagonale \ %%%

gagneDiag1Test(X,Y,J,Rf,Pf,Af) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheTestHautVerif(X1,Y1,J,Rg,Pg,Ag),
	incr(X,X2),
	decr(Y,Y2),
	droiteTestBasVerif(X2,Y2,J,Rd,Pd,Ad),
	!,
	Rf is Rg+Rd, Pf is Pg+Pd, Af is Ag+Ad.

gaucheTestHautVerif(X,Y,J,Rg,Pg,Ag):-
	gaucheTestHaut(X,Y,J,0,Rg,Pg,Ag).
gaucheTestHaut(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectDiag1(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
gaucheTestHaut(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gaucheTestHaut(X,Y,J,R,Rg,Pg,Ag) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheTestHaut(X1,Y1,J,R1,Rg,Pg,Ag).

droiteTestBasVerif(X,Y,J,Rg,Pg,Ag):-
	droiteTestBas(X,Y,J,0,Rg,Pg,Ag).
droiteTestBas(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectDiag1(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
droiteTestBas(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droiteTestBas(X,Y,J,R,Rg,Pg,Ag) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteTestBas(X1,Y1,J,R1,Rg,Pg,Ag).

%%% En diagonale / %%%

gagneDiag2Test(X,Y,J,Rf,Pf,Af) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheTestBasVerif(X1,Y1,J,Rg,Pg,Ag),
	incr(X,X2),
	incr(Y,Y2),
	droiteTestHautVerif(X2,Y2,J,Rd,Pd,Ad),
	!,
	Rf is Rg+Rd, Pf is Pg+Pd, Af is Ag+Ad.

gaucheTestBasVerif(X,Y,J,Rg,Pg,Ag) :-
	gaucheTestBas(X,Y,J,0,Rg,Pg,Ag).
gaucheTestBas(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectDiag2(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
gaucheTestBas(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
gaucheTestBas(X,Y,J,R,Rg,Pg,Ag) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheTestBas(X1,Y1,J,R1,Rg,Pg,Ag).

droiteTestHautVerif(X,Y,J,Rg,Pg,Ag) :-
	droiteTestHaut(X,Y,J,0,Rg,Pg,Ag).
droiteTestHaut(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectDiag2(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation?
droiteTestHaut(X,Y,J,R,R,0,0) :-
	not(caseTest(X,Y,J)). %Jusqu'à la caseTest non J
droiteTestHaut(X,Y,J,R,Rg,Pg,Ag) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteTestHaut(X1,Y1,J,R1,Rg,Pg,Ag).

%%%%% gagneTestDirect %%%%%


gagneTestDirect(X,Y,J) :-
	gagneTestDirectLigne(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag1(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag2(X,Y,J).
gagneTestDirect(X,Y,J) :-
    gagneTestDirectColonne(X,Y,J).

%%% En ligne %%%

gagneTestDirectLigne(X,Y,J) :-
	decr(X,X1),
	gaucheVerif(X1,Y,J,Rg),
	incr(X,X2),
	droiteVerif(X2,Y,J,Rd),
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

%%% En colonne %%%

gagneTestDirectColonne(X,Y,J) :-
    decr(Y,Y1),
    caseTest(X,Y1,J),
    decr(Y1,Y2),
    caseTest(X,Y2,J),
    decr(Y2,Y3),
    caseTest(X,Y3,J).

%%%%%%% caseVideTest %%%%%

caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).
