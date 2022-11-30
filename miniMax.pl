%%%%%%%%%%%% miniMax.pl %%%%%%%%%%%%
% Implémentation de minimax avec diverses optimisations propres au Puissance 4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(miniMax, [parcoursArbre/5, caseTest/3, gagneTest/4]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).
:- use_module(eval).
:- use_module(ia).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic caseTest/3.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parcoursArbre/5(+Joueur,+ProfondeurMax,+ChoixAlgo,-MeilleureColonne,-MeilleurScore)
parcoursArbre(J,Pmax,1,BestX,BestScore) :-
	initCaseTest,assert(maximizer(J)), assert(joueurCourant(J)),
	nbColonnes(NBCOLONNES),
	bestScoreCol(NBCOLONNES,1,Pmax,BestScore,BestXList),
	random_member(BestX, BestXList),
	%write("BestX: "), write(BestX), write(" Score: "), write(BestScore), nl,
	clearTest,!.

parcoursArbre(J,Pmax,2,BestX,BestScore) :-
	initCaseTest,assert(maximizer(J)), assert(joueurCourant(J)),
	nbColonnes(NBCOLONNES),
	infiniteNeg(Alpha),
	infinitePos(Beta),
	bestScoreColAB(NBCOLONNES,1,Pmax,BestScore,BestXList,Alpha,Beta),
	random_member(BestX, BestXList),
	%write("BestX: "), write(BestX), write(" Score: "), write(BestScore), nl,
	clearTest,!.

%%%%%%%%%%%%%%%% MINIMAX avec elagage ALPHA/BETA %%%%%%%%%%%%%%%%

% bestScoreColAB/7(+Colonne,+Profondeur,+ProfondeurMax,-MeilleurScore,-MeilleuresColonne,+Alpha,+Beta)
bestScoreColAB(1,P,Pmax,BestScore,[1],Alpha,Beta) :-
	parcoursAB(1,P,Pmax,BestScore,Alpha,Beta).
bestScoreColAB(X,P,Pmax,BestScore,BestXList,Alpha,Beta) :-
	parcoursAB(X,P,Pmax,ScoreX,Alpha,Beta),
	%write("X: "),write(X),write(" Score: "),write(ScoreX),nl,
	coupureAB(X,ScoreX,P,Pmax,BestXList,BestScore,Alpha,Beta).

% coupureAB/8(+Colonne,+ScoreCplonne,+Profondeur,+ProfondeurMax,-MeilleuresColonnes,-MeilleurScore,+Alpha,+Beta).
% MeilleuresColonnes correspond à la liste des colonnes, de la colonne X à 1, qui ont donnent un score égale à MeilleurScore
coupureAB(X,ScoreX,_,_,[X],ScoreX,_,Beta):-
	% coupure Beta
	joueurCourant(J), maximizer(J),
	ScoreX > Beta.
coupureAB(X,ScoreX,P,Pmax,BestXList,BestScore,Alpha,Beta):-
	joueurCourant(J), maximizer(J),
	decr(X,X1),
	NewAlpha is max(Alpha,ScoreX),
	bestScoreColAB(X1,P,Pmax,BestScoreX1,BestX1List,NewAlpha,Beta),
	compScore(X,ScoreX,BestX1List,BestScoreX1,BestXList,BestScore).
coupureAB(X,ScoreX,_,_,[X],ScoreX,Alpha,_):-
	% coupure Alpha
	joueurCourant(J), not(maximizer(J)),
	ScoreX < Alpha.
coupureAB(X,ScoreX,P,Pmax,BestXList,BestScore,Alpha,Beta):-
	joueurCourant(J), not(maximizer(J)),
	decr(X,X1),
	NewBeta is min(Beta,ScoreX),
	bestScoreColAB(X1,P,Pmax,BestScoreX1,BestX1List,Alpha,NewBeta),
	compScore(X,ScoreX,BestX1List,BestScoreX1,BestXList,BestScore).

% parcoursAB/6(+Colonne,+Profondeur,+Pmax,-Score,+Alpha,+Beta)
parcoursAB(X,_,_,ScoreX,_,_) :- nbLignes(NBLIGNES),case(X,NBLIGNES,_), joueurCourant(Joue), maximizer(Joue), infiniteNeg(2,ScoreX).
parcoursAB(X,_,_,ScoreX,_,_) :- nbLignes(NBLIGNES),case(X,NBLIGNES,_), joueurCourant(Joue), not(maximizer(Joue)), infinitePos(2,ScoreX).
parcoursAB(X,_,_,ScoreX,_,_) :- nbLignes(NBLIGNES),caseTest(X,NBLIGNES,_), joueurCourant(Joue), evaluate(X,NBLIGNES,Joue,ScoreX).
parcoursAB(X, P,_, ScoreX,_,_):-
	testVictoireDirecte(TestVictoireDirecte), TestVictoireDirecte>0,
	joueurCourant(Joue), calculPositionJeton(X, 1, Y), gagneTest(X,Y,Joue,Direct), victoireDirecte(X,Y,Joue,P,Direct,ScoreX).
parcoursAB(X,Pmax,Pmax,ScoreX,_,_) :- joueurCourant(Joue), placerJeton(X,Y,Joue), evaluate(X, Y, Joue, ScoreX), retract(caseTest(X,Y,Joue)).
parcoursAB(X,P,Pmax,BestScore,Alpha,Beta) :-
	%write("X: "),write(X),write(" P: "),write(P),nl,
	joueurCourant(J),
	placerJeton(X,Y,J),
	nbColonnes(NBCOLONNES),
	incr(P,NewP),
	changerJoueur,
	bestScoreColAB(NBCOLONNES,NewP,Pmax,BestScore,_,Alpha,Beta),
	changerJoueur,
	retract(caseTest(X,Y,J)).


%%%%%%%%%%%%%%%% MINIMAX sans elagage ALPHA/BETA %%%%%%%%%%%%%%%%

% bestScoreCol/5(+Colonne,+Profondeur,+Pmax,-MeilleurScore,-MeilleuresColonnes)
bestScoreCol(1,P,Pmax,BestScore,[1]) :-
	parcours(1,P,Pmax,BestScore).
	%write("X: "),write(1),write(" Score: "),write(BestScore),nl,!.
bestScoreCol(X,P,Pmax,BestScore,BestXList) :- % pour afficher le score final pour chaque colonne
	parcours(X,P,Pmax,ScoreX),
	%write("X: "),write(X),write(" Score: "),write(ScoreX),nl,
	decr(X,X1),
	bestScoreCol(X1,P,Pmax,BestScoreX1,BestX1List),
	compScore(X,ScoreX,BestX1List,BestScoreX1,BestXList,BestScore).
bestScoreCol(X,P,Pmax,BestScore,BestXList) :-
	parcours(X,P,Pmax,ScoreX),
	%write("P: "),write(Pmax),write(" X: "),write(X),write(" Score: "),write(ScoreX),nl,
	decr(X,X1),
	bestScoreCol(X1,P,Pmax,BestScoreX1,BestX1List),
	compScore(X,ScoreX,BestX1List,BestScoreX1,BestXList,BestScore).

% parcours/4(+Colonne,+Profondeur,+Pmax,-Score)
parcours(X,_,_,ScoreX) :- nbLignes(NBLIGNES),case(X,NBLIGNES,_), joueurCourant(Joue), maximizer(Joue), infiniteNeg(2,ScoreX).
parcours(X,_,_,ScoreX) :- nbLignes(NBLIGNES),case(X,NBLIGNES,_), joueurCourant(Joue), not(maximizer(Joue)), infinitePos(2,ScoreX).
parcours(X,_,_,ScoreX) :- nbLignes(NBLIGNES),caseTest(X,NBLIGNES,_), joueurCourant(Joue), evaluate(X,NBLIGNES,Joue,ScoreX).
parcours(X, P,_, ScoreX):-
	testVictoireDirecte(TestVictoireDirecte), TestVictoireDirecte>0,
	joueurCourant(Joue), calculPositionJeton(X, 1, Y), gagneTest(X,Y,Joue,Direct), victoireDirecte(X,Y,Joue,P,Direct,ScoreX).
parcours(X,Pmax,Pmax,ScoreX) :- joueurCourant(Joue), placerJeton(X,Y,Joue), evaluate(X, Y, Joue, ScoreX), retract(caseTest(X,Y,Joue)).
parcours(X,P,Pmax,BestScore) :-
	%write("X: "),write(X),write(" P: "),write(P),nl,
	joueurCourant(J),
	placerJeton(X,Y,J),
	nbColonnes(NBCOLONNES),
	incr(P,NewP),
	changerJoueur,
	bestScoreCol(NBCOLONNES,NewP,Pmax,BestScore,_),
	changerJoueur,
	retract(caseTest(X,Y,J)).



% compScore/6(+ColonneX,+ScoreX,+MeilleuresColonnesX1,+MeilleurScoreX1,-MeilleuresColonnes,-MeilleurScore)
% MeilleuresColonnes est la liste des colonnes qui donnent le MeilleurScore (minimum ou maximum en fonction du joueur)
compScore(X,ScoreX,X1List,ScoreX,[X|X1List],ScoreX) :- !.
compScore(X,ScoreX,_,ScoreX1,[X],ScoreX) :-
	joueurCourant(J), maximizer(J),
	ScoreX > ScoreX1, !.
compScore(_,_,X1List,ScoreX1,X1List,ScoreX1) :-
	joueurCourant(J), maximizer(J), !.
compScore(X,ScoreX,_,ScoreX1,[X],ScoreX) :-
	ScoreX < ScoreX1, !.
compScore(_,_,X1List,ScoreX1,X1List,ScoreX1) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

victoireDirecte(_,_,J,P,1,Value):- maximizer(J), Pp is 1-P, infinitePos(Pp,Value). %Victoire du max
victoireDirecte(_,_,J,P,1,Value):- not(maximizer(J)), Pp is 1-P, infiniteNeg(Pp,Value). %Victoire du min

victoireDirecte(X,Y,J,_,0,_):- assert(caseTest(X,Y,J)), false.

victoireDirecte(X,Y,J,P,0,Value):- maximizer(J), Pp is -P, infinitePos(Pp,Value),
	autreJoueur(J2), testDefaiteProchaine(J2),
	retract(caseTest(X,Y,J)). %Victoire anticipée du max
victoireDirecte(X,Y,J,P,0,Value):- not(maximizer(J)), Pp is -P, infiniteNeg(Pp,Value),
	autreJoueur(J2), testDefaiteProchaine(J2),
	retract(caseTest(X,Y,J)). %Victoire anticipée du min

victoireDirecte(X,Y,J,_,0,_):- retract(caseTest(X,Y,J)), false. %ménage si on perde derrière

victoireDirecte(X,Y,J,_,-5,_):- assert(caseTest(X,Y,J)), false.

victoireDirecte(X,Y,J,P,-5,Value):- maximizer(J), Pp is -5-P, infinitePos(Pp,Value),
	autreJoueur(J2), testDefaiteAnticipeeProchaine(J2),
	retract(caseTest(X,Y,J)). %Victoire anticipée du max
victoireDirecte(X,Y,J,P,-5,Value):- not(maximizer(J)), Pp is -5-P, infiniteNeg(Pp,Value),
	autreJoueur(J2), testDefaiteAnticipeeProchaine(J2),
	retract(caseTest(X,Y,J)). %Victoire anticipée du min

victoireDirecte(X,Y,J,_,-5,_):- retract(caseTest(X,Y,J)), false. %ménage si on perde derrière


%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

initCaseTest :- case(X,Y,Z), assert(caseTest(X,Y,Z)), false. %on assert une caseTest pour toutes les cases.
initCaseTest.

clearTest :-
	retractall(caseTest(X,_,_)),
	retract(maximizer(X)), retract(joueurCourant(_)). % on eve tout ce que l'on a ajouté.

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



%%%%%%%%%% Aide calcul. Redundant code, do not remove %%%%%%%%%%%%%


changerJoueur:- joueurCourant(jaune), retract(joueurCourant(_)), assert(joueurCourant(rouge)).
changerJoueur:- joueurCourant(rouge), retract(joueurCourant(_)), assert(joueurCourant(jaune)).

autreJoueur(rouge):- joueurCourant(jaune).
autreJoueur(jaune):- joueurCourant(rouge).


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
	Pf is P2+P3+P4, % cases adjacentes vides jouables autour des lignes du joueur (de 0 à 6) - case vide au dessus (colonne) exclue
	Af is A1+A2+A3+A4, % lorsque case vide jouable, indique si la case du dessus est aussi gagnante (de 0 à 7)
	testFinal(R1,R2,R3,R4,Pf,Af,V), % V s'unifie à 1 si directement gagnant, 0 si au moins 2 cases adj vides jouables gagnantes (ie gagnant au prochain tour), -5 si au moins 1 accumulation (ie gagnant au prochain tour aussi) (faux dans les autres cas)
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

% (+X,+Y,+J,-NCaseAdjJoueur,-IsCasesVidesAdjJouables,-IsCasesVidesAdjGagnantes)
gagneLigneTest(X,Y,J,Rf,Pf,Af) :-
	decr(X,X1),
	gaucheTestVerif(X1,Y,J,Rg,Pg,Ag),
	incr(X,X2),
	droiteTestVerif(X2,Y,J,Rd,Pd,Ad),
	!,
	Rf is Rg+Rd, Pf is Pg+Pd, Af is Ag+Ad.

% (+X,+Y,+J,-NCaseAdjGaucheJoueur,-IsCaseVideGaucheJouable,-IsCaseVideGaucheGagnante)
gaucheTestVerif(X,Y,J,Rg,Pg,Ag):-
	gaucheTest(X,Y,J,0,Rg,Pg,Ag).
gaucheTest(X,Y,J,R,R,Pg,Ag):-
	caseTestValideVide(X,Y),	%case dans le tableau et vide
	gagneTestDirectLigne(X,Y,J),	%gagnante
	testPotentielAccumulation(X,Y,J,Pg,Ag).		%Peut on la placer et a-t-on accumulation? (case X/Y est jouable directement (Pg=1) et case du dessus/dessous permet de ganger directement (Ag=1))
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
