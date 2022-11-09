%%%%%%%%%%%% testIAs.pl %%%%%%%%%%%%
% Permet de tester comment performent différentes IAs lorsqu'elles jouent l'une contre l'autre.

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(ia).
:- use_module(eval).
:- use_module(miniMax).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ['websimulate.pl'].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic joueurCourant/2.
:- dynamic autreJoueur/2.

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% runTest/3
% NbIterations: le nombre de parties à jouer,
% IA1 et IA2 : les identifiants des 2 IA à confronter
% IA1 joue contre IA2 "NbIterations" fois le predicat affiche combien de fois qui a battu qui
% On cherche à évaluer le nbr de coups moyen qu'il a fallu pour gagner
runTest(NbIterations,IA1,IA2) :-
	NbIterationsParIA is NbIterations//2,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	typeJoueurPreconf(IA1,TypeIA1,_,_,_),
	typeJoueurPreconf(IA2,TypeIA2,_,_,_),
	runTestIAXEnPremier(NbIterationsParIA,IA1,IA2,0,NbFoisIA1GagneEnCommencant,0,NbFoisIA1PerdEnCommencant,NCoupIA1P1,NCoupIA2P1),
	write("Stats sur "),write(NbIterationsParIA),write(" parties où IA1 ("),write(TypeIA1),write(") commence: "),nl,
	IA1MoyP1 is NCoupIA1P1/NbIterationsParIA,
	IA2MoyP1 is NCoupIA2P1/NbIterationsParIA,
	write("	IA1 - Nombre moyen de coups joués par partie: "),write(IA1MoyP1),nl,
	write("	IA2 - Nombre moyen de coups joués par partie: "),write(IA2MoyP1),nl,
	write("	IA1vsIA2 - Victoires/Défaites: "),write(NbFoisIA1GagneEnCommencant),write("/"),write(NbFoisIA1PerdEnCommencant),nl,
	runTestIAXEnPremier(NbIterationsParIA,IA2,IA1,0,NbFoisIA2GagneEnCommencant,0,NbFoisIA2PerdEnCommencant,NCoupIA2P2,NCoupIA1P2),
	write("Stats sur "),write(NbIterationsParIA),write(" parties où IA2 ("),write(TypeIA2),write(") commence: "),nl,
	IA1MoyP2 is NCoupIA1P2/NbIterationsParIA,
	IA2MoyP2 is NCoupIA2P2/NbIterationsParIA,
	write("	IA1 - Nombre moyen de coups joués par partie: "),write(IA1MoyP2),nl,
	write("	IA2 - Nombre moyen de coups joués par partie: "),write(IA2MoyP2),nl,
	write("	IA1vsIA2 - Victoires/Défaites: "),write(NbFoisIA2PerdEnCommencant),write("/"),write(NbFoisIA2GagneEnCommencant),nl,
	%assert(statsIA1(Dict1.get(wall), Dict1.get(inferences), Dict2.get(wall), Dict2.get(inferences))),
	% STATS_IA1 = {TEMPS_SI_IA1_COMMENCE, NB_ITERATIONS_SI_IA1_COMMENCE, TEMPS_SI_IA1_NE_COMMENCE_PAS, NB_ITERATIONS_SI_IA1_NE_COMMENCE_PAS}

	%assert(statsIA2(Dict2.get(wall), Dict2.get(inferences), STATS_IA1(Dict2.get(wall), Dict1.get(inferences)))),
	% STATS_IA2 = {TEMPS_SI_IA2_COMMENCE, NB_ITERATIONS_SI_IA2_COMMENCE, TEMPS_SI_IA2_NE_COMMENCE_PAS, NB_ITERATIONS_SI_IA2_NE_COMMENCE_PAS}
	statistics,
	!.

benchmark(NbIterations, IA1, IA2) :-
	call_time(runTest(NbIterations, IA1, IA2),Dict),
	tmoy is Dict.get(wall) / NbIterations,
	write('Temps moyen : '), write(tmoy),
	nl,
	write('Temps total : '), write(tmoy * NbIterations),
	nl,
	statistics, %for printing threads' information like allocated memory for instance, number of inferences, etc...
	!.
	
%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gestion Data Benchmark %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%resetTimerIAs :- retract(ia1Timer(X)), retract(ia2Timer(Y)), assert()


% temps moyen avant de gagner
%calculTmpMoy(IA, NbIterations) :-


% test de sortie de runTestIAXEnPremier
runTestIAXEnPremier(0,_,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni,0,0).

runTestIAXEnPremier(NbIterations,IA1,IA2,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin,NCoupIA1,NCoupIA2) :-
	init,
	assert(joueurCourant(rouge,IA1)), % rouge = IA1
	assert(autreJoueur(jaune,IA2)),
	jeu(PartieNulle,NCoupIA1Partie,NCoupIA2Partie),
	joueurCourant(CouleurIAGagnante,_),
	incrementerGagnant(PartieNulle,CouleurIAGagnante,NbIA1GagneIni,NbIA1GagneFin1,NbIA2GagneIni,NbIA2GagneFin1),
	NbIterations2 is NbIterations-1,
	runTestIAXEnPremier(NbIterations2,IA1,IA2,NbIA1GagneFin1,NbIA1GagneFin,NbIA2GagneFin1,NbIA2GagneFin,NCoupIA1PartiesSuivantes,NCoupIA2PartiesSuivantes),
	NCoupIA1 is NCoupIA1PartiesSuivantes + NCoupIA1Partie,
	NCoupIA2 is NCoupIA2PartiesSuivantes + NCoupIA2Partie.

incrementerGagnant(true,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni).
%+,+,+,-,+,-
incrementerGagnant(false,rouge,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneIni) :-
	NbIA1GagneFin is NbIA1GagneIni+1.
%+,+,+,-,+,-
incrementerGagnant(false,jaune,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneFin) :-
	NbIA2GagneFin is NbIA2GagneIni+1.

jeu(PartieNulle,NCoupIA1,NCoupIA2) :-
	tour(PartieNulle,NCoupIA1,NCoupIA2).

tour(PartieNulle,NCoupIA1,NCoupIA2) :-
	joueurCourant(CouleurJCourant,TypeJoueur),
	obtenirCoupPreconf(CouleurJCourant,TypeJoueur,Coup),
	placerJeton(Coup,Y,CouleurJCourant),
	testFin(Coup,Y,CouleurJCourant, PartieNulle,NCoupIA1Fin,NCoupIA2Fin),
	incrJoueurCoup(CouleurJCourant,NCoupIA1Fin,NCoupIA2Fin,NCoupIA1,NCoupIA2).

testFin(Coup,Y,CouleurJCourant,false,0,0) :-
	gagne(Coup,Y,CouleurJCourant).
testFin(_,_,_,true,0,0) :-
	not(coupPossible).
testFin(_,_,_,PartieNulle,NCoupIA1,NCoupIA2) :-
	changerJoueurPreconf,
	tour(PartieNulle,NCoupIA1,NCoupIA2).

incrJoueurCoup(rouge,NCoupIA1Fin,NCoupIA2Fin,NCoupIA1,NCoupIA2Fin) :-
	NCoupIA1 is NCoupIA1Fin+1.
incrJoueurCoup(jaune,NCoupIA1Fin,NCoupIA2Fin,NCoupIA1Fin,NCoupIA2) :-
	NCoupIA2 is NCoupIA2Fin+1.

obtenirCoupPreconf(_,2,Colonne) :-
	iaAleatoire(Colonne), !.
obtenirCoupPreconf(CouleurJCourant,JoueurPreconf,Colonne) :-
	typeJoueurPreconf(JoueurPreconf,_,ChoixAlgo,ListEval,Profondeur),
	iaMinimax(CouleurJCourant,Colonne,Profondeur,ChoixAlgo,ListEval).


changerJoueurPreconf :-
	joueurCourant(rouge,TypeJoueurR),
	autreJoueur(jaune,TypeJoueurJ),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)),
	assert(joueurCourant(jaune,TypeJoueurJ)),
	assert(autreJoueur(rouge,TypeJoueurR)),!.
changerJoueurPreconf :-
	joueurCourant(jaune,TypeJoueurJ),
	autreJoueur(rouge,TypeJoueurR),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)),
	assert(joueurCourant(rouge,TypeJoueurR)),
	assert(autreJoueur(jaune,TypeJoueurJ)),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init :-
	initJeu,
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)).
