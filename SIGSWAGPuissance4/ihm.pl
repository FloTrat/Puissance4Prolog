%%%%%%%%%%%%%%
%% DÉPRÉCIÉ %%
%%%%%%%%%%%%%%

%%%%%%%%%%%% ihm.pl %%%%%%%%%%%%

:- module(ihm, [afficher/0, demandeCoup/3, afficherGagnant/4, afficherPartieNulle/0, demandeTypeDeJeu/1]).

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

% demandeTypeDeJeu/1(-TypeDeJeu)
% Demande à l'utilisateur de saisir un type de jeu. N'échoue pas en cas d'entrée invalide (en dehors des valeurs possibles).
% TypeDeJeu s'unifie au type saisi par l'utilisateur.
demandeTypeDeJeu(TypeDeJeu) :-
    write('   --- Puissance 4 ---'), nl,
	findall(_, afficherTypeJoueur(_,_), _),
    nl, nl,
	write(' ----------------------- '), nl,
    write('Saisissez votre choix :'), nl,
    read(TypeDeJeu), integer(TypeDeJeu).


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
	Y is 7-Y1,
	findall(_, afficherLigne(_,Y), _),
	nl.

afficherLigne(X,Y) :-
	nbColonnes(NbColonnes),
	between(1,NbColonnes,X),
	afficherCase(X,Y).

afficherCase(X,Y) :- case(X,Y,rouge), write(r), !.
afficherCase(X,Y) :- case(X,Y,jaune), write(j), !.
afficherCase(_,_) :- write(.).

saisirCoup(Coup) :-
	write('Veuillez saisir votre coup : '),
	read(Coup).

afficherTypeJoueur(I,J) :-
	typeJoueur(I,J),
	write('\t'), write(I), write('. '), write(J), nl.
