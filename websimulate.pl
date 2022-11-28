%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Adaptation du code 'webserver.pl' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%% websimulate.pl %%%%%%%%%%%%
% Serveur web permettant d'interfacer Prolog avec notre IHM console.

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%
:- use_module(jeu).
:- use_module(ia).
:- use_module(eval).
:- use_module(ihm).


%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%
%%  Actions

% initAction
% Initialise le jeu, son plateau et ses joueurs.
% Répond une liste des joueurs disponibles ainsi que leur code correspondant
initAction :-
    initJeu,
    retractall(joueurCourant(_,_,_)),
    retractall(autreJoueur(_,_,_)),
	demandeTypeDeJeu(TypeJoueurR, TypeEvalJoueurR),
	demandeTypeDeJeu(TypeJoueurJ, TypeEvalJoueurJ),
    % random_select(TypeJoueurR,[TypeJoueur1,TypeJoueur2],[TypeJoueurJ|_]),
    assert(joueurCourant(rouge,TypeJoueurR, TypeEvalJoueurR)),
    assert(autreJoueur(jaune,TypeJoueurJ, TypeEvalJoueurJ)),
	tourAction.


% tourAction/0
% Récupère des parametres GET la colonne jouées par un Humain, Vérifie la validité du coup, et le joue
% Répond un gameStatus à "invalid" si le coup joué n'est pas valide (colonne pleine, ou hors limite), ou
% Répond la colonne et la ligne où le jeton a pu être inséré,
% ainsi qu'un gameStatus à
%   "draw" si la partie se termine sur une égalité,
%   "continue" si la partie n'est pas terminée (dans ce cas le joueur courant est changé),
%   "win" si le coup a amené à une victoire.

% tourAction/0
% ! Le joueurCourant doit être une IA donc la manière de jouée est renseignée par le prédicat 'obtenirCoup'
% Demande à l'IA courante de jouer un coup valide, puis place ce jeton.
% Répond la colonne et la ligne où le jeton a pu être inséré,
% ainsi qu'un gameStatus à
%   "draw" si la partie se termine sur une égalité,
%   "continue" si la partie n'est pas terminée (dans ce cas le joueur courant est changé),
%   "win" si le coup a amené à une victoire.
tourAction :-
    joueurCourant(CouleurJCourant,TypeJoueur,ListEval),
	typeJoueur(TypeJoueur,Type),
	write('C\'est au joueur '), write(CouleurJCourant), write(' ('), write(Type), write(') de jouer.'),  nl,
	obtenirCoup(CouleurJCourant,TypeJoueur,ListEval,Colonne),
    placerJeton(Colonne,Ligne,CouleurJCourant),
	afficher,
	write('Joueur '), write(CouleurJCourant), write(' vient de jouer dans la colonne '), write(Colonne), write('.'),  nl,
    statutJeu(Colonne,Ligne,CouleurJCourant).



% obtenirCoup/4(+CouleurJCourant,+CodeIA,+ListEval,-Colonne)
% Unifie à Colonne le coup joué par l'IA dont le code est CodeIA
obtenirCoup(_,1,_,Colonne) :-
	write('Saisissez votre colonne :'), nl,
    read(Colonne), integer(Colonne).

obtenirCoup(_,2,_,Colonne) :-
	iaAleatoire(Colonne).

obtenirCoup(CouleurJCourant,11,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,1,1,ListEval).
obtenirCoup(CouleurJCourant,12,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,2,1,ListEval).
obtenirCoup(CouleurJCourant,13,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,3,1,ListEval).
obtenirCoup(CouleurJCourant,14,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,4,1,ListEval).
obtenirCoup(CouleurJCourant,15,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,5,1,ListEval).
obtenirCoup(CouleurJCourant,21,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,1,2,ListEval).
obtenirCoup(CouleurJCourant,22,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,2,2,ListEval).
obtenirCoup(CouleurJCourant,23,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,3,2,ListEval).
obtenirCoup(CouleurJCourant,24,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,4,2,ListEval).
obtenirCoup(CouleurJCourant,25,ListEval,Colonne) :-
	iaMinimax(CouleurJCourant,Colonne,5,2,ListEval).

%%%%%%%%%%%%%%%
%%  Utilitaires

% statutJeu/1(+Colonne,+Ligne,+CouleurJCourant,-Statut)
% Unifie à Statut l'état du jeu,
% avec comme dernier coup joué : un jeton en Colonne Ligne par CouleurJCourant
% Status s'unifie à "win" si le coup a amené à une victoire.
statutJeu(Colonne,Ligne,CouleurJCourant) :-
    gagne(Colonne,Ligne,CouleurJCourant), write('Joueur '), write(CouleurJCourant), write(' a gagné!').
% Status s'unifie à "draw" si la partie se termine sur une égalité,
statutJeu(_,_,_) :-
    not(coupPossible), write('draw').
% Status s'unifie à "continue" si la partie n'est pas terminée (dans ce cas le joueur courant est changé),
statutJeu(_,_,_) :-
    changerJoueur,
	tourAction.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%