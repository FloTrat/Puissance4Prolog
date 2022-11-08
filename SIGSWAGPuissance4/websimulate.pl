%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Adaptation du code 'webserver.pl' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%% websimulate.pl %%%%%%%%%%%%
% Serveur web permettant d'interfacer Prolog avec notre IHM console.

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%
:- use_module(jeu).
:- use_module(ia).
:- use_module(eval).
:- use_module(ihm).


%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%
%%  Actions

% initAction/1(+Request)
% Initialise le jeu, son plateau et ses joueurs.
% Répond une liste des joueurs disponibles ainsi que leur code correspondant
initAction :-
    initJeu,
    retractall(joueurCourant(_,_,_)),
    retractall(autreJoueur(_,_,_)),
	selectionnerJoueurAction.

memberRetInt(X, [], Res) :- Res = 0.
memberRetInt(X, [X|_], Res) :- Res = 1.
memberRetInt(X, [_|Y], Res) :- memberRetInt(X,Y, Res).
% selectionnerJoueurAction/1(+Request)
% Récupère des parametres GET les joueurs sélectionnés les unifie les unifie à leur couleurs
% Le joueur rouge étant le joueurCourant, c'est lui qui commencera à jouer.
% Répond les couleurs et le code de chaqun des joueurs
selectionnerJoueurAction :-
	demandeTypeDeJeu(TypeJoueurR, TypeEvalJoueurR),
	demandeTypeDeJeu(TypeJoueurJ, TypeEvalJoueurJ),
    % random_select(TypeJoueurR,[TypeJoueur1,TypeJoueur2],[TypeJoueurJ|_]),
    assert(joueurCourant(rouge,TypeJoueurR, TypeEvalJoueurR)),
    assert(autreJoueur(jaune,TypeJoueurJ, TypeEvalJoueurJ)),

    memberRetInt(1,TypeEvalJoueur, EvalConf),
    memberRetInt(2,TypeEvalJoueur, EvalPosition),
    memberRetInt(3,TypeEvalJoueur, EvalPuissances3),
    memberRetInt(4,TypeEvalJoueur, EvalDensite),
    memberRetInt(5,TypeEvalJoueur, EvalAdjacence),
    memberRetInt(6,TypeEvalJoueur, EvalTest),
    memberRetInt(7,TypeEvalJoueur, EvalAlea),

    assert(poidsConf(EvalConf)),
    assert(poidsPosition(EvalPosition)),
    assert(poidsPuissance3(EvalPuissances3)),
    assert(poidsDensite(EvalDensite)),
    assert(poidsAdjacence(EvalAdjacence)),
    assert(poidsTest(EvalTest)),
    assert(poidsAlea(EvalAlea)),

	tourAction.

    retract(poidsConf(EvalConf)),
    retract(poidsPosition(EvalPosition)),
    retract(poidsPuissance3(EvalPuissances3)),
    retract(poidsDensite(EvalDensite)),
    retract(poidsAdjacence(EvalAdjacence)),
    retract(poidsTest(EvalTest)),
    retract(poidsAlea(EvalAlea)).

tourAction :-
    joueurCourant(CouleurJCourant,TypeJoueur, TypeEvalJoueur),
    joueurCourant(CouleurJCourant,1,TypeEvalJoueur),
	validerTourHumain(CouleurJCourant),
	!.

tourAction :-
	tourIAAction.


% validerTourHumain/1(+Request)
% Récupère des parametres GET la colonne jouées par un Humain, Vérifie la validité du coup, et le joue
% Répond un gameStatus à "invalid" si le coup joué n'est pas valide (colonne pleine, ou hors limite), ou
% Répond la colonne et la ligne où le jeton a pu être inséré,
% ainsi qu'un gameStatus à
%   "draw" si la partie se termine sur une égalité,
%   "continue" si la partie n'est pas terminée (dans ce cas le joueur courant est changé),
%   "win" si le coup a amené à une victoire.
validerTourHumain(CouleurJCourant) :-
	typeJoueur(1,Type),
	write('C\'est au joueur '), write(CouleurJCourant), write(' ('), write(Type), write(') de jouer.'),  nl,
	write('Saisissez votre colonne :'), nl,
    read(Colonne), integer(Colonne),
    placerJeton(Colonne, Ligne, CouleurJCourant),
	afficher,
	write('Joueur '), write(CouleurJCourant), write(' vient de jouer dans la colonne '), write(Colonne), write('.'),  nl,
    statutJeu(Colonne,Ligne,CouleurJCourant),
    !.
%validerTourHumain(_) :-
%    reply_json(json{correct:true, gameStatus:invalid}).

% tourIAAction/1(+Request)
% ! Le joueurCourant doit être une IA donc la manière de jouée est renseignée par le prédicat 'obtenirCoup'
% Demande à l'IA courante de jouer un coup valide, puis place ce jeton.
% Répond la colonne et la ligne où le jeton a pu être inséré,
% ainsi qu'un gameStatus à
%   "draw" si la partie se termine sur une égalité,
%   "continue" si la partie n'est pas terminée (dans ce cas le joueur courant est changé),
%   "win" si le coup a amené à une victoire.
tourIAAction :-
    joueurCourant(CouleurJCourant,TypeJoueur,TypeEvalJoueur),
	typeJoueur(TypeJoueur,Type),
	write('C\'est au joueur '), write(CouleurJCourant), write(' ('), write(Type), write(') de jouer.'),  nl,
    obtenirCoup(CouleurJCourant,TypeJoueur,Colonne),
    placerJeton(Colonne,Ligne,CouleurJCourant),
	afficher,
	write('Joueur '), write(CouleurJCourant), write(' vient de jouer dans la colonne '), write(Colonne), write('.'),  nl,
    statutJeu(Colonne,Ligne,CouleurJCourant).


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

% obtenirCoup/1(+CouleurJCourant,+CodeIA,-Coup)
% Unifie à Colonne le coup joué par l'IA dont le code est CodeIA
% CodeIA == 2 :- IA aleatoire
obtenirCoup(_,2,TypeEvalJoueur,Coup) :-
    iaAleatoire(Coup).

obtenirCoup(CouleurJCourant,3,Coup) :-
    iaMinimax(CouleurJCourant,Coup,1,0).
obtenirCoup(CouleurJCourant,4,Coup) :-
    iaMinimax(CouleurJCourant,Coup,2,0).
obtenirCoup(CouleurJCourant,5,Coup) :-
    iaMinimax(CouleurJCourant,Coup,3,0).
obtenirCoup(CouleurJCourant,6,Coup) :-
    iaMinimax(CouleurJCourant,Coup,4,0).
obtenirCoup(CouleurJCourant,7,Coup) :-
    iaMinimax(CouleurJCourant,Coup,5,0).
obtenirCoup(CouleurJCourant,8,Coup) :-
    iaMinimax(CouleurJCourant,Coup,1,1).
obtenirCoup(CouleurJCourant,9,Coup) :-
    iaMinimax(CouleurJCourant,Coup,2,1).
obtenirCoup(CouleurJCourant,10,Coup) :-
    iaMinimax(CouleurJCourant,Coup,3,1).
obtenirCoup(CouleurJCourant,11,Coup) :-
    iaMinimax(CouleurJCourant,Coup,4,1).
obtenirCoup(CouleurJCourant,12,Coup) :-
    iaMinimax(CouleurJCourant,Coup,5,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%