%%%%%%%%%%%% ia.pl %%%%%%%%%%%%
% Deux "moteurs" d'IA :
% - "Aléatoire" jouant aléatoirement ;
% - "Minimax", implémentation de minimax assez paramétrable.

:- module(ia, [iaAleatoire/1
			  ,iaMinimax/11
			  ,poidsPuissance3/1
			  ,poidsPosition/1
			  ,poidsDensite/1
			  ,poidsAdjacence/1
			  ,poidsAlea/1
			  ,poidsTest/1
			  ,poidsConf/1]
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
:- dynamic poidsAlea/1.
:- dynamic poidsTest/1.
:- dynamic poidsConf/1.

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

iaMinimax(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsPuissance3,PoidsDensite,PoidsAdjacence,Alea,PoidsTest,PoidsConf,ChoixAlgo) :-
	assert(poidsPosition(PoidsPosition)),
	assert(poidsPuissance3(PoidsPuissance3)),
	assert(poidsDensite(PoidsDensite)),
	assert(poidsAdjacence(PoidsAdjacence)),
	assert(poidsAlea(Alea)),
	assert(poidsTest(PoidsTest)),
	assert(poidsConf(PoidsConf)),
	parcoursArbre(JoueurCourant,Profondeur,ChoixAlgo,Coup,_),
	retract(poidsPosition(PoidsPosition)),
	retract(poidsPuissance3(PoidsPuissance3)),
	retract(poidsDensite(PoidsDensite)),
	retract(poidsAdjacence(PoidsAdjacence)),
	retract(poidsAlea(Alea)),
	retract(poidsTest(PoidsTest)),
	retract(poidsConf(PoidsConf)).
