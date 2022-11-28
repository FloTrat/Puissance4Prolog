%%%%%%%%%%%% testIAs.pl %%%%%%%%%%%%
% Permet de tester comment performent différentes IAs lorsqu'elles jouent l'une contre l'autre.

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(ia).
:- use_module(eval).
:- use_module(miniMax).
:- use_module(util).

:- ['websimulate.pl'].

:- dynamic joueurCourant/3.
:- dynamic autreJoueur/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% runTest/3
% NbIterations: le nombre de parties à jouer,
% IA1 et IA2 : les identifiants des 2 IA à confronter
% IA1 joue contre IA2 "NbIterations" fois le predicat affiche combien de fois qui a battu qui
% On cherche à évaluer le nbr de coups moyen qu'il a fallu pour gagner
runTest(NbIterations,IA1,IA2) :-
	NbIterationsParIA is NbIterations//2,
	runTestIAXEnPremier(NbIterationsParIA,IA1,IA2,0,NbFoisIA1GagneEnCommencant,0,NbFoisIA1PerdEnCommencant,NCoupIA1P1,NCoupIA2P1,TpsP1IA1,TpsP1IA2),
	write('Si IA1 commence : '),
	afficheKmoy(NCoupIA1P1,NCoupIA2P1, NbIterationsParIA), %lorsqu'IA1 commence
	afficheStatsTemporelle(TpsP1IA1, NbIterationsParIA),
	afficheStatsTemporelle(TpsP1IA2, NbIterationsParIA),
	runTestIAXEnPremier(NbIterationsParIA,IA2,IA1,0,NbFoisIA2GagneEnCommencant,0,NbFoisIA2PerdEnCommencant,NCoupIA1P2,NCoupIA2P2,TpsP2IA1,TpsP2IA2),
	nl, write('Si IA2 commence : '),
	afficheKmoy(NCoupIA1P2,NCoupIA2P2, NbIterationsParIA), %lorsqu'IA2 commence
	afficheStatsTemporelle(TpsP2IA1, NbIterationsParIA),
	afficheStatsTemporelle(TpsP2IA2, NbIterationsParIA),
	nl,
	typeJoueurPreconf(IA1,NomIA1,_,_),
	typeJoueurPreconf(IA2,NomIA2,_,_),
	%assert(statsIA1(Dict1.get(wall), Dict1.get(inferences), Dict2.get(wall), Dict2.get(inferences))),
	% STATS_IA1 = {TEMPS_SI_IA1_COMMENCE, NB_ITERATIONS_SI_IA1_COMMENCE, TEMPS_SI_IA1_NE_COMMENCE_PAS, NB_ITERATIONS_SI_IA1_NE_COMMENCE_PAS}

	%assert(statsIA2(Dict2.get(wall), Dict2.get(inferences), STATS_IA1(Dict2.get(wall), Dict1.get(inferences)))),
	% STATS_IA2 = {TEMPS_SI_IA2_COMMENCE, NB_ITERATIONS_SI_IA2_COMMENCE, TEMPS_SI_IA2_NE_COMMENCE_PAS, NB_ITERATIONS_SI_IA2_NE_COMMENCE_PAS}
	write(NomIA2), write(' (jaune) en commençant : a gagné '), write(NbFoisIA2GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA2PerdEnCommencant),write(' fois.'),
	nl,
	write(NomIA1), write(' (rouge) en commençant : a gagné '), write(NbFoisIA1GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA1PerdEnCommencant),write(' fois.'),
	!.


afficheKmoy(NCoupIA1,NCoupIA2, NbIterations) :-
 M1 is (NCoupIA1 / NbIterations), write('KMoy(IA1)='), write(M1), write(' '),
 M2 is (NCoupIA2 / NbIterations), write('KMoy(IA2)='), write(M2), write(' ').

afficheStatsTemporelle(TpsTotal, NbIterations) :-
 M1 is (TpsTotal / NbIterations), write('Tps(IA)='), write(M1), write('s'), write(' ').

runTestIAXEnPremier(0,_,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni,0,0,0,0) :- !.

runTestIAXEnPremier(NbIterations,IA1,IA2,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin,NCoupIA1,NCoupIA2,TpsPartieXIA1,TpsPartieXIA2) :-
	init,
	assert(joueurCourant(rouge,IA1,[])), % rouge = IA1
	assert(autreJoueur(jaune,IA2,[])),
	jeu(PartieNulle,NCoupIA1Partie,NCoupIA2Partie,TpsPartieXIA1Jeu,TpsPartieXIA2Jeu),
	%write('Tps partiel : '), write(TpsPartieXIA1), nl,
	joueurCourant(CouleurIAGagnante,_,_),
	incrementerGagnant(PartieNulle,CouleurIAGagnante,NbIA1GagneIni,NbIA1GagneFin1,NbIA2GagneIni,NbIA2GagneFin1),
	NbIterations2 is NbIterations-1,
	runTestIAXEnPremier(NbIterations2,IA1,IA2,NbIA1GagneFin1,NbIA1GagneFin,NbIA2GagneFin1,NbIA2GagneFin,NCoupIA1PartiesSuivantes,NCoupIA2PartiesSuivantes,TpsSuivPartieXIA1,TpsSuivPartieXIA2),
	NCoupIA1 is NCoupIA1PartiesSuivantes + NCoupIA1Partie,
	NCoupIA2 is NCoupIA2PartiesSuivantes + NCoupIA2Partie,
	TpsPartieXIA1 is TpsSuivPartieXIA1 + TpsPartieXIA1Jeu,
	TpsPartieXIA2 is TpsSuivPartieXIA2 + TpsPartieXIA2Jeu.

jeu(PartieNulle,NCoupIA1,NCoupIA2,TpsPartieXIA1,TpsPartieXIA2) :-
	tour(PartieNulle,NCoupIA1,NCoupIA2,TpsPartieXIA1,TpsPartieXIA2).

tour(PartieNulle,NCoupIA1,NCoupIA2,TpsPartieXIA1,TpsPartieXIA2) :-
	joueurCourant(CouleurJCourant,IA,_),
	typeJoueurPreconf(IA,_,CodeIA,ListEval),
	call_time(obtenirCoup(CouleurJCourant,CodeIA,ListEval,Coup), DictDataTemporellePartieXPourIAX),
	placerJeton(Coup,Y,CouleurJCourant),
	testFin(Coup,Y,CouleurJCourant, PartieNulle,NCoupIA1Fin,NCoupIA2Fin,TpsIniPartieXIA1,TpsIniPartieXIA2),
	tpsPartielStats(CouleurJCourant, TpsIniPartieXIA1, TpsPartieXIA1,TpsIniPartieXIA2,TpsPartieXIA2,DictDataTemporellePartieXPourIAX),
	incrJoueurCoup(CouleurJCourant,NCoupIA1Fin,NCoupIA2Fin,NCoupIA1,NCoupIA2).

tpsPartielStats(rouge, TpsPartieXIA1,TpsSuivPartieXIA1,TpsPartieXIA2,TpsPartieXIA2, Dico) :-
	TpsSuivPartieXIA1 is TpsPartieXIA1 + Dico.get(wall).

tpsPartielStats(jaune,TpsPartieXIA1,TpsPartieXIA1,TpsPartieXIA2,TpsSuivPartieXIA2,Dico) :-
	TpsSuivPartieXIA2 is TpsPartieXIA2 + Dico.get(wall).

testFin(Coup,Y,CouleurJCourant,false,0,0,0,0) :-
	gagne(Coup,Y,CouleurJCourant).
testFin(_,_,_,true,0,0,0,0) :-
	not(coupPossible).
testFin(_,_,_,PartieNulle,NCoupIA1,NCoupIA2,TpsPartieXIA1,TpsPartieXIA2) :-
	changerJoueur,
	tour(PartieNulle,NCoupIA1,NCoupIA2,TpsPartieXIA1,TpsPartieXIA2).

incrJoueurCoup(rouge,NCoupIA1Fin,NCoupIA2Fin,NCoupIA1,NCoupIA2Fin) :-
	NCoupIA1 is NCoupIA1Fin+1.
incrJoueurCoup(jaune,NCoupIA1Fin,NCoupIA2Fin,NCoupIA1Fin,NCoupIA2) :-
	NCoupIA2 is NCoupIA2Fin+1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

incrementerGagnant(true,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni).
%+,+,+,-,+,-
incrementerGagnant(false,rouge,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneIni) :-
	NbIA1GagneFin is NbIA1GagneIni+1.
%+,+,+,-,+,-
incrementerGagnant(false,jaune,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneFin) :-
	NbIA2GagneFin is NbIA2GagneIni+1.

init :-
	initJeu,
	retractall(joueurCourant(_,_,_)),
	retractall(autreJoueur(_,_,_)).
