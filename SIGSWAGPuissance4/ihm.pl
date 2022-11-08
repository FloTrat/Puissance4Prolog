﻿%%%%%%%%%%%%%%
%% DÉPRÉCIÉ %%
%%%%%%%%%%%%%%

%%%%%%%%%%%% ihm.pl %%%%%%%%%%%%

:- module(ihm, [afficher/0, demandeCoup/3, afficherGagnant/4, afficherPartieNulle/0, demandeTypeDeJeu/2]).

:- use_module(util).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% afficher/0
% Affiche dans la console la partie actuelle.
% Tout le temps vrai.
afficher :-
	findall(_, afficherColonne(_), _),
	nl,
	findall(_, afficherPlateau(_), _).

% demandeCoup/3(+CouleurJCourant, +Message, -Coup)
% Demande à l'utilisateur courant de saisir un coup.
% Coup s'unifie au coup saisi par l'utilisateur.
demandeCoup(CouleurJCourant, Message, Coup) :-
	nl, write(Message), nl,
	write('['), write(CouleurJCourant), write('] '),
	saisirCoup(Coup).

% afficherGagnant/4(+CouleurGagnante, +CouleurPerdante, +TypeJoueurGagnant, +TypeJoueurPerdant)
% Affiche le gagnant et le perdant.
% Tout le temps vrai.
afficherGagnant(CouleurGagnante, CouleurPerdante, TypeJoueurGagnant, TypeJoueurPerdant) :-
	nl,
	write('Le joueur '),
	write(TypeJoueurGagnant),
	write(' ('), write(CouleurGagnante), write(')'),
	write(' gagne contre le joueur '),
	write(TypeJoueurPerdant),
	write(' ('),write(CouleurPerdante),write(')').

% afficherPartieNulle/0
% Affiche qu'il y a partie nulle.
% Tout le temps vrai.
afficherPartieNulle :-
	nl,
	write('Il y a egalite entre les 2 joueurs').

readEval(ListEval) :- read(X), evals(X,ListEval).
evals(end,[]) :- !.
evals(X,[X|ListEval]) :- read(Y), evals(Y,ListEval).

emptyList([]).

% demandeTypeDeJeu/1(-TypeDeJeu)
% Demande à l'utilisateur de saisir un type de jeu. N'échoue pas en cas d'entrée invalide (en dehors des valeurs possibles).
% TypeDeJeu s'unifie au type saisi par l'utilisateur.
demandeTypeDeJeu(TypeIA, TypeEval) :-
	write('   --- Puissance 4 ---'), nl,
	write('   --- Veuillez choisir une IA ---'), nl,
	findall(_, afficherTypeJoueur(_,_), _),
	nl, nl,
	write(' ----------------------- '), nl,
    write('Saisissez votre choix :'), nl,
    read(TypeIA), integer(TypeIA),

	((TypeIA =\= 1, TypeIA =\= 2) ->
		nl,
		write(' ----------------------- '), nl,
		write('   --- Veuillez choisir une ou plusieurs heuristiques ---'), nl,
		write('   --- ex: Pour choisir l''heuristique 1, entrer ''1.'' puis ''end.'' ---'), nl,
		findall(_, afficherTypeHeuristique(_,_), _),
		write(' ----------------------- '), nl,
		readEval(TypeEval)
	;emptyList(TypeEval)).
	

%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

afficherColonne(X) :-
	nbColonnes(NbColonnes),
	between(1,NbColonnes,X),
	write(X).

% principe : on parcourt la base de faits et pour chaque case on affiche une couleur (ou pas)
afficherPlateau(Y) :-
	nbLignes(NbLignes),
	between(1,NbLignes,Y1),
	Y is NbLignes+1-Y1,
	findall(_, afficherLigne(_,Y), _),
	nl.

afficherLigne(X,Y) :-
	nbColonnes(NbColonnes),
	between(1,NbColonnes,X),
	afficherCase(X,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
afficherCase(X,Y) :- case(X,Y,rouge), write("\033[31m"), write(o), write("\033[0m"),!.
afficherCase(X,Y) :- case(X,Y,jaune), write("\033[33m"), write(x), write("\033[0m"),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


afficherCase(_,_) :- write(.).

saisirCoup(Coup) :-
	write('Veuillez saisir votre coup : '),
	read(Coup).

afficherTypeJoueur(I,J) :-
	typeJoueur(I,J),
	write('\t'), write(I), write('. '), write(J), nl.

afficherTypeHeuristique(I,J) :-
	typeHeuristique(I,J),
	write('\t'), write(I), write('. '), write(J), nl.