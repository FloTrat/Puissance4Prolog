%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
% Deux "moteurs" d'IA :
% - "Aléatoire" jouant aléatoirement ;
% - "Minimax", implémentation de minimax assez paramétrable.

:- module(ia, [iaAleatoire/1
			  ,iaMinimax/5
			  ,poidsPuissance3/1
			  ,poidsPosition/1
			  ,poidsDensite/1
			  ,poidsAdjacence/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			  ,pertubations/1
			  ,poidsTest/1
			  ,poidsConf/1
			  ,testVictoireDirecte/1]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
).


%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(util).
:- use_module(miniMax).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats dynamiques %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic poidsPuissance3/1.
:- dynamic poidsPosition/1.
:- dynamic poidsDensite/1.
:- dynamic poidsAdjacence/1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic poidsTest/1.
:- dynamic poidsConf/1.
:- dynamic pertubations/1.
:- dynamic testVictoireDirecte/1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

iaAleatoire(Coup) :-
	nbColonnes(NBCOLONNES),
	Coup is random(NBCOLONNES)+1,
	coupValide(Coup).
% AI Aléatoire a choisi une colonne pleine, donc on la fait recommencer.
iaAleatoire(Coup) :-
	iaAleatoire(Coup).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% iaMinimax(+Joueur,-Coup,+ProfondeurMax,+ChoixAlgo,+ListEval)
iaMinimax(JoueurCourant,Coup,Profondeur,ChoixAlgo,ListEval) :-
	memberRetInt(0,ListEval, TestVictoireDirecte),
	memberRetInt(1,ListEval, EvalConf),
    memberRetInt(2,ListEval, EvalPosition),
    memberRetInt(3,ListEval, EvalPuissances3),
    memberRetInt(4,ListEval, EvalDensite),
    memberRetInt(5,ListEval, EvalAdjacence),
    memberRetInt(6,ListEval, EvalTest),
    memberRetInt(7,ListEval, Pertubations),

    assert(testVictoireDirecte(TestVictoireDirecte)),
    assert(poidsConf(EvalConf)),
    assert(poidsPosition(EvalPosition)),
    assert(poidsPuissance3(EvalPuissances3)),
    assert(poidsDensite(EvalDensite)),
    assert(poidsAdjacence(EvalAdjacence)),
    assert(poidsTest(EvalTest)),
    assert(pertubations(Pertubations)),
	parcoursArbre(JoueurCourant,Profondeur,ChoixAlgo,Coup,_),
    retract(testVictoireDirecte(TestVictoireDirecte)),
    retract(poidsConf(EvalConf)),
    retract(poidsPosition(EvalPosition)),
    retract(poidsPuissance3(EvalPuissances3)),
    retract(poidsDensite(EvalDensite)),
    retract(poidsAdjacence(EvalAdjacence)),
    retract(poidsTest(EvalTest)),
    retract(pertubations(Pertubations)).


memberRetInt(_, [], 0).
memberRetInt(X, [X|_], 1).
memberRetInt(X, [_|Y], Res) :- memberRetInt(X,Y, Res).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%