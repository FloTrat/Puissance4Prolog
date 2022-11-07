%%%%%%%%%%%% testIAs.pl %%%%%%%%%%%%
% Permet de tester comment performent différentes IAs lorsqu'elles jouent l'une contre l'autre.

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(jeu).
:- use_module(ia).
:- use_module(eval).
:- use_module(miniMax).
:- ['webserver.pl'].

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
	call_time(runTestIAXEnPremier(NbIterationsParIA,IA1,IA2,0,NbFoisIA1GagneEnCommencant,0,NbFoisIA1PerdEnCommencant), Dict1),
	call_time(runTestIAXEnPremier(NbIterationsParIA,IA2,IA1,0,NbFoisIA2GagneEnCommencant,0,NbFoisIA2PerdEnCommencant), Dict2),
	typeJoueur(IA1,TypeIA1),
	typeJoueur(IA2,TypeIA2),
	assert(STATS_IA1(Dict1.get(wall), Dict1.get(inferences), Dict2.get(wall), Dict2.get(inferences))),
	% STATS_IA1 = {TEMPS_SI_IA1_COMMENCE, NB_ITERATIONS_SI_IA1_COMMENCE, TEMPS_SI_IA1_NE_COMMENCE_PAS, NB_ITERATIONS_SI_IA1_NE_COMMENCE_PAS}

	assert(STATS_IA2(Dict2.get(wall), Dict2.get(inferences), STATS_IA1(Dict2.get(wall), Dict1.get(inferences)))),
	% STATS_IA2 = {TEMPS_SI_IA2_COMMENCE, NB_ITERATIONS_SI_IA2_COMMENCE, TEMPS_SI_IA2_NE_COMMENCE_PAS, NB_ITERATIONS_SI_IA2_NE_COMMENCE_PAS}
	write(TypeIA2), write(' en commençant : a gagné '), write(NbFoisIA2GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA2PerdEnCommencant),write(' fois.'),
	nl,
	write(TypeIA1), write(' en commençant : a gagné '), write(NbFoisIA1GagneEnCommencant),write(' fois et a perdu '),write(NbFoisIA1PerdEnCommencant),write(' fois.'),
	statistics,
	!.

benchmark(NbIterations, IA1, IA2) :-
	call_time(runTest(NbIterations, IA1, IA2),Dict),
	Tmoy is Dict.get(wall) / NbIterations
	write('Temps moyen : '), write(Tmoy),
	nl,
	write('Temps total : '), write(Tmoy * NbIterations),
	nl,
	statistics, %for printing threads' information like allocated memory for instance, number of inferences, etc...
	!.
	
%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gestion Data Benchmark %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

saveNbRuns(X) :- assert(testRuns(X)).

resetCoupsIAs :- retract(ia1Coups(X)), retract(ia2Coups(Y)), assert(ia1Coups(0)), assert(ia2Coups(0)).

%resetTimerIAs :- retract(ia1Timer(X)), retract(ia2Timer(Y)), assert()

incrementCoupsIA(IA) :-
 joueurCourant(X, IA),
 X = rouge,
 	ia1Coups(S), E is S+1, retract(ia1Coups(S)), assert(ia1Coups(E));
 X = jaune,
 	ia2Coups(S), E is S+1, retract(ia2Coups(S)), assert(ia2Coups(E)).

% temps moyen avant de gagner
%calculTmpMoy(IA, NbIterations) :-


calculKMoy(IA, NbIterations) :- 
 joueurCourant(X, IA),
 X = rouge, ia1Coups(S), write('KMoy pour '), write(IA), M is S / NbIterations, write(M);
 X = jaune, ia2Coups(S), write('KMoy pour '), write(IA), M is S / NbIterations, write(M).

% test de sortie de runTestIAXEnPremier
runTestIAXEnPremier(0,_,_,NbIA1GagneIni,NbIA1GagneIni,NBNbIA2GagneIni,NbIA2GagneIni) :-
    calculKMoy(IA1, testRuns(X)),
	calculKMoy(IA2, testRuns(X)),
	!.

runTestIAXEnPremier(NbIterations,IA1,IA2,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneFin) :-
	saveNbRuns(NbIterations), % va être lancé à chaque fois mais ne sera assert que la première fois
	init,
	resetCoupsIAs,
	assert(joueurCourant(rouge,IA1)), % rouge = IA1
	assert(autreJoueur(jaune,IA2)),
	jeu(PartieNulle),
	joueurCourant(CouleurIAGagnante,_),
	incrementerGagnant(PartieNulle,CouleurIAGagnante,NbIA1GagneIni,NbIA1GagneFin1,NbIA2GagneIni,NbIA2GagneFin1),
	NbIterations2 is NbIterations-1,
	runTestIAXEnPremier(NbIterations2,IA1,IA2,NbIA1GagneFin1,NbIA1GagneFin,NbCoupsIA1,NbIA2GagneFin1,NbIA2GagneFin,NbCoupsIA2).

incrementerGagnant(true,_,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneIni).
incrementerGagnant(false,rouge,NbIA1GagneIni,NbIA1GagneFin,NbIA2GagneIni,NbIA2GagneIni) :-
	NbIA1GagneFin is NbIA1GagneIni+1.
incrementerGagnant(false,jaune,NbIA1GagneIni,NbIA1GagneIni,NbIA2GagneIni,NbIA2GagneFin) :-
	NbIA2GagneFin is NbIA2GagneIni+1.

jeu(PartieNulle) :-
	tour(PartieNulle).

tour(PartieNulle) :-
	joueurCourant(CouleurJCourant,TypeJoueur),
	obtenirCoup(CouleurJCourant,TypeJoueur,Coup),
	placerJeton(Coup,Y,CouleurJCourant),
	incrementCoupsIA(TypeJoueur),
	testFin(Coup,Y,CouleurJCourant, PartieNulle).

testFin(Coup,Y,CouleurJCourant,PartieNulle) :-
	gagne(Coup,Y,CouleurJCourant),
	PartieNulle=false.
testFin(_,_,_,PartieNulle) :-
	not(coupPossible),
	PartieNulle=true.
testFin(_,_,_,PartieNulle) :-
	changerJoueur,
	tour(PartieNulle).

init :-
	initJeu,
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)).
