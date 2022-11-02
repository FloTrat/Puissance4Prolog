%%%%%%%%%%%% webserver.pl %%%%%%%%%%%%
% Serveur web permettant d'interfacer Prolog avec notre IHM web.

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
    retractall(joueurCourant(_,_)),
    retractall(autreJoueur(_,_)),
	selectionnerJoueurAction.

% selectionnerJoueurAction/1(+Request)
% Récupère des parametres GET les joueurs sélectionnés les unifie les unifie à leur couleurs
% Le joueur rouge étant le joueurCourant, c'est lui qui commencera à jouer.
% Répond les couleurs et le code de chaqun des joueurs
selectionnerJoueurAction :-
	demandeTypeDeJeu(TypeJoueurR),
	demandeTypeDeJeu(TypeJoueurJ),
    % random_select(TypeJoueurR,[TypeJoueur1,TypeJoueur2],[TypeJoueurJ|_]),
    assert(joueurCourant(rouge,TypeJoueurR)),
    assert(autreJoueur(jaune,TypeJoueurJ)),
	tourAction.

tourAction :-
    joueurCourant(CouleurJCourant,1),
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
    joueurCourant(CouleurJCourant,_),
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
    joueurCourant(CouleurJCourant,TypeJoueur),
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
obtenirCoup(_,2,Coup) :-
    iaAleatoire(Coup).

obtenirCoup(CouleurJCourant,3,Coup) :-
    % iaMinimax(JoueurCourant,Coup,Profondeur,PoidsPosition,PoidsPuissance3,PoidsDensite,PoidsAdjacence)
    iaMinimax(CouleurJCourant,Coup,3,1,0,0,0).
obtenirCoup(CouleurJCourant,4,Coup) :-
    iaMinimax(CouleurJCourant,Coup,3,1,0,0,0).
obtenirCoup(CouleurJCourant,5,Coup) :-
    iaMinimax(CouleurJCourant,Coup,4,1,0,0,0).
obtenirCoup(CouleurJCourant,6,Coup) :-
    iaMinimax(CouleurJCourant,Coup,4,1,1,0,0).
obtenirCoup(CouleurJCourant,7,Coup) :-
    iaMinimax(CouleurJCourant,Coup,5,1,1,0,0).
obtenirCoup(CouleurJCourant,8,Coup) :-
    iaMinimax(CouleurJCourant,Coup,5,1,1,0,0).
