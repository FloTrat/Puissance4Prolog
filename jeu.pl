%%%%%%%%%%%% jeu.pl %%%%%%%%%%%%

:- module(jeu, [
	initJeu/0,
	gagne/3,
	placerJeton/3,
	coupPossible/0,
	case/3,
	coupValide/1,
	typeJoueur/2,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	typeJoueurPreconf/4,
	changerJoueur/0,
	insererJeton/3,
	typeHeuristique/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
]).


%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic case/3.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gestion des heuristiques %%%
% Définitiion des heuristiques.
% typeHeuristique/2(+Code,-Nom)
typeHeuristique(0, 'victoireAnticipee').
typeHeuristique(1, 'evalConf').
typeHeuristique(2, 'evalPosition').
typeHeuristique(3, 'evalPuissances3').
typeHeuristique(4, 'densite').
typeHeuristique(5, 'evalAdjacence').
typeHeuristique(6, 'evalTest').
typeHeuristique(7, 'pertubations').

%%% Gestion des joueurs %%%

% Définition des différentes IA pour le jeu
% typeJoueur/2(+CodeIA,-NomIA)
typeJoueur(1,'Humain').
typeJoueur(2,'IA Aléatoire').
typeJoueur(11,'IA Minimax 1 - Algo HeptarchieCorp').
typeJoueur(12,'IA Minimax 2 - Algo HeptarchieCorp').
typeJoueur(13,'IA Minimax 3 - Algo HeptarchieCorp').
typeJoueur(14,'IA Minimax 4 - Algo HeptarchieCorp').
typeJoueur(15,'IA Minimax 5 - Algo HeptarchieCorp').
typeJoueur(21,'IA Minimax 1 α/β - Algo HeptarchieCorp').
typeJoueur(22,'IA Minimax 2 α/β - Algo HeptarchieCorp').
typeJoueur(23,'IA Minimax 3 α/β - Algo HeptarchieCorp').
typeJoueur(24,'IA Minimax 4 α/β - Algo HeptarchieCorp').
typeJoueur(25,'IA Minimax 5 α/β - Algo HeptarchieCorp').

% Définition d'IA préconfigurées pour les tests
% typeJoueurPreconf/4(+CodePreconf,-NomIA,-CodeIA,-ListEval)
typeJoueurPreconf(2,'IA Aléatoire',2,[]).
typeJoueurPreconf(3,'IA Minimax 3 α/β - victoireAnticipee + eval Position',23,[0,2]).
typeJoueurPreconf(4,'IA Minimax 3 α/β - victoireAnticipee + eval Position+Puissance3',23,[0,2,3]).
typeJoueurPreconf(5,'IA Minimax 4 α/β - victoireAnticipee + eval Position',24,[0,2]).
typeJoueurPreconf(6,'IA Minimax 4 α/β - victoireAnticipee + eval Position+Puissance3',24,[0,2,3]).
typeJoueurPreconf(7,'IA Minimax 5 α/β - victoireAnticipee + eval Position',25,[0,2]).
typeJoueurPreconf(8,'IA Minimax 5 α/β - victoireAnticipee + eval Position+Puissance3',25,[0,2,3]).
typeJoueurPreconf(9,'IA Minimax 3 α/β - eval test',23,[6]).
typeJoueurPreconf(10,'IA Minimax 4 α/β - eval test',24,[6]).
typeJoueurPreconf(11,'IA Minimax 5 α/β - eval test',25,[6]).
typeJoueurPreconf(12,'IA Minimax 1 α/β - eval Configuration',21,[1]).
typeJoueurPreconf(13,'IA Minimax 2 α/β - eval Configuration',22,[1]).
typeJoueurPreconf(14,'IA Minimax 3 α/β - eval Configuration',23,[1]).
typeJoueurPreconf(15,'IA Minimax 4 α/β - eval Configuration',24,[1]).
typeJoueurPreconf(16,'IA Minimax 5 α/β - eval Configuration',25,[1]).
typeJoueurPreconf(17,'IA Minimax 1 - eval Configuration',11,[1]).
typeJoueurPreconf(18,'IA Minimax 2 - eval Configuration',12,[1]).
typeJoueurPreconf(19,'IA Minimax 3 - eval Configuration',13,[1]).

changerJoueur :-
	joueurCourant(rouge,TypeJoueurR, TypeEvalJoueurR),
	autreJoueur(jaune,TypeJoueurJ, TypeEvalJoueurJ),
	retractall(joueurCourant(_,_,_)),
	retractall(autreJoueur(_,_,_)),
	assert(joueurCourant(jaune,TypeJoueurJ, TypeEvalJoueurJ)),
	assert(autreJoueur(rouge,TypeJoueurR, TypeEvalJoueurR)),!.
changerJoueur :-
	joueurCourant(jaune,TypeJoueurJ, TypeEvalJoueurJ),
	autreJoueur(rouge,TypeJoueurR, TypeEvalJoueurR),
	retractall(joueurCourant(_,_,_)),
	retractall(autreJoueur(_,_,_)),
	assert(joueurCourant(rouge,TypeJoueurR, TypeEvalJoueurR)),
	assert(autreJoueur(jaune,TypeJoueurJ, TypeEvalJoueurJ)),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% Prédicats utiles

%%% Initialisation du plateau

% initJeu/0
% vide le plateau, initialise un nouveau plateau vide
% retourne yes
initJeu :-
	initClear.

% coupPossible/0
% Vérifie si l'on peut encore joueur.
% Vrai s'il reste des coups valides, faux sinon
coupPossible :-
	nbColonnes(NBCOLONNES),
	between(1,NBCOLONNES,X),
	coupValide(X).

%%% Vérification de la victoire

% gagne/3(+Colonne, +Ligne, +Joueur)
% Vérifie si le coup est gagnant pour joueur.
% Vrai si gagnant.
gagne(X,Y,J) :-
	gagneColonne(X,Y,J).
gagne(X,Y,J) :-
	gagneLigne(X,Y,J).
gagne(X,Y,J) :-
	gagneDiag1(X,Y,J).
gagne(X,Y,J) :-
	gagneDiag2(X,Y,J).


%%% Place un jeton

% placerJeton/3(-Colonne, +Ligne, -Couleur)
% ins�re si possible un jeton dans la colonne donn�e
% retourne la ligne d'insertion, ou no
placerJeton(X,Y,C) :-
	coupValide(X),
	insererJeton(X, Y, C).

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%


%%%%% init %%%%%


initClear :-
	retractall(case(_,_,_)). % pourrait fonctionner avec :- dynamic, � investiguer

initTest :-
	assert(case(4,1,rouge)),
	assert(case(3,2,rouge)),
	assert(case(2,3,rouge)),
	assert(case(1,4,rouge)). %initInterface, play


%%%%% gagne %%%%%


%%% En colonne %%%

gagneColonne(X,Y,J) :-
	case(X,Y,J),
	decr(Y,Y1),
	case(X,Y1,J),
	decr(Y1,Y2),
	case(X,Y2,J),
	decr(Y2,Y3),
	case(X,Y3,J). %ligne en bas

%%% En ligne %%%

gagneLigne(X,Y,J) :-
	gaucheVerif(X,Y,J,Rg),
	droiteVerif(X,Y,J,Rd),
	!,
	Rf is Rg+Rd, Rf>4.

gaucheVerif(X,Y,J,Rg):-
	gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'� la case non J
gauche(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):-
	droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'� la case non J
droite(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneDiag1(X,Y,J) :-
	gaucheHautVerif(X,Y,J,Rg),
	droiteBasVerif(X,Y,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>4.

gaucheHautVerif(X,Y,J,Rg):-
	gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'� la case non J
gaucheHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):-
	droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'� la case non J
droiteBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneDiag2(X,Y,J) :-
	gaucheBasVerif(X,Y,J,Rg),
	droiteHautVerif(X,Y,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>4.

gaucheBasVerif(X,Y,J,Rg) :-
	gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'� la case non J
gaucheBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg) :-
	droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :-
	not(case(X,Y,J)). %Jusqu'à la case non J
droiteHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg).


%%%%% placerJeton %%%%%


% coupValide/1(-Colonne)
% Vérifie si un jeton est jouable dans cette colonne
% retourne yes ou no
coupValide(X) :-
	nbColonnes(NBCOLONNES),
	X=<NBCOLONNES,
	X>=1,
	nbLignes(NBLIGNES),
	caseVide(X,NBLIGNES).

% insererJeton/3(-Colonne, +Ligne, -Couleur)
% Insère, sans vérification, un jeton de la couleur donnée, dans la colonne donnée.
% Y s'unifie à la ligne jouée.
insererJeton(X,Y,C) :-
	calculPositionJeton(X, 1, Y),
	assert(case(X,Y,C)).

% calculPositionJeton/3(+Colonne,+LigneToCheck,-Ligne)
% Calcule la première ligne vide d'une colonne.
% Retourne l'indice de cette ligne vide.
calculPositionJeton(X,YCheck,YCheck) :-
	caseVide(X,YCheck),
	!.
calculPositionJeton(X,YCheck,Y) :-
	incr(YCheck, YCheck1),
	calculPositionJeton(X,YCheck1,Y).
