%%%%%%%%%%%% webserver.pl %%%%%%%%%%%%
% Serveur web permettant d'interfacer Prolog avec notre IHM web.

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(jeu).
:- use_module(ia).
:- use_module(eval).


%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% start/0
% start lance le serveur sur le port 8000.
% Est vrai lorsque le webserver est correctement démarré
start :- serveur(8000).

% serveur/1(+Port)
% Démarre le serveur, rendant accessible l'interface web à http://localhost:"Port"
% Est vrai lorsque le webserver est correctement démarré
serveur(Port) :- http_server(http_dispatch, [port(Port)]).


%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%

% Déclarations dynamiques, multifiles et constantes de location
:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(files, '/f', []).


%%%%%%%%%%%
%%  Routage

% Affiliation d'un prédicat/1(+Request) à une route
% Lorsqu'une requête est récupérée sur l'une des route suivante, le prédicat correspondant est appelé.
:- http_handler('/', helloAction, []).
:- http_handler('/init', initAction, []).
:- http_handler('/selectPlayers', selectionnerJoueurAction, []).
:- http_handler('/playFromIA', tourIAAction, []).
:- http_handler('/validHumanPlay', validerTourHumain, []).
:- http_handler(files(.), fichierAction, [prefix]).
:- http_handler('/game', indexAction, []).


%%%%%%%%%%%
%%  Actions

% indexAction/1(+Request)
% Affiche la page d'accueil du jeu.
% Répond la page index.html située dans le dossier web/pages
indexAction(Request) :-
    http_reply_from_files('web/pages', [], Request).
indexAction(Request) :-
    http_404([], Request).

% helloAction/1(+Request)
% Affiche le message "Hello world".
% Répond un message attestant du fonctionnement du serveur
helloAction(_) :-
    format('Content-type: text/plain~n~n'),
    format('Hello world ! Server is running').

% initAction/1(+Request)
% Initialise le jeu, son plateau et ses joueurs.
% Répond une liste des joueurs disponibles ainsi que leur code correspondant
initAction(_) :-
    initJeu,
    retractall(joueurCourant(_,_)),
    retractall(autreJoueur(_,_)),
    findall([X,Y], typeJoueur(X,Y), Z),
    reply_json(json{correct:true, players:Z}).

% fichierAction/1(+Request)
% Sert les fichiers publics demandé sous l'url /f/fichier.extension.
% Répond le fichier demandé situé dans le dossier web/assets
fichierAction(Request) :-
    http_reply_from_files('web/assets', [], Request).
fichierAction(Request) :-
    http_404([], Request).

% selectionnerJoueurAction/1(+Request)
% Récupère des parametres GET les joueurs sélectionnés les unifie les unifie à leur couleurs
% Le joueur rouge étant le joueurCourant, c'est lui qui commencera à jouer.
% Répond les couleurs et le code de chaqun des joueurs
selectionnerJoueurAction(Request) :-
    http_parameters(Request,
        [ joueur1(TypeJoueurR, []),
          joueur2(TypeJoueurJ, [])
        ]),
    % random_select(TypeJoueurR,[TypeJoueur1,TypeJoueur2],[TypeJoueurJ|_]),
    atom_number(TypeJoueurR, TypeJoueurRInteger),
    atom_number(TypeJoueurJ, TypeJoueurJInteger),
    assert(joueurCourant(rouge,TypeJoueurRInteger)),
    assert(autreJoueur(jaune,TypeJoueurJInteger)),
    reply_json(json{correct:true, rouge:TypeJoueurR, jaune:TypeJoueurJ}).

% validerTourHumain/1(+Request)
% Récupère des parametres GET la colonne jouées par un Humain, Vérifie la validité du coup, et le joue
% Répond un gameStatus à "invalid" si le coup joué n'est pas valide (colonne pleine, ou hors limite), ou
% Répond la colonne et la ligne où le jeton a pu être inséré,
% ainsi qu'un gameStatus à
%   "draw" si la partie se termine sur une égalité,
%   "continue" si la partie n'est pas terminée (dans ce cas le joueur courant est changé),
%   "win" si le coup a amené à une victoire.
validerTourHumain(Request) :-
    http_parameters(Request,[ col(Col, [])]),
    atom_number(Col, Colonne),
    joueurCourant(CouleurJCourant,_),
    placerJeton(Colonne, Ligne, CouleurJCourant),
    statutJeu(Colonne,Ligne,CouleurJCourant, Statut),
    reply_json(json{correct:true, gameStatus:Statut, colPlayed:Colonne, rowPlayed:Ligne}),
    !.
validerTourHumain(_) :-
    reply_json(json{correct:true, gameStatus:invalid}).

% tourIAAction/1(+Request)
% ! Le joueurCourant doit être une IA donc la manière de jouée est renseignée par le prédicat 'obtenirCoup'
% Demande à l'IA courante de jouer un coup valide, puis place ce jeton.
% Répond la colonne et la ligne où le jeton a pu être inséré,
% ainsi qu'un gameStatus à
%   "draw" si la partie se termine sur une égalité,
%   "continue" si la partie n'est pas terminée (dans ce cas le joueur courant est changé),
%   "win" si le coup a amené à une victoire.
tourIAAction(_) :-
    joueurCourant(CouleurJCourant,TypeJoueur),
    obtenirCoup(CouleurJCourant,TypeJoueur,Colonne),
    placerJeton(Colonne,Ligne,CouleurJCourant),
    statutJeu(Colonne,Ligne,CouleurJCourant, Statut),
    reply_json(json{correct:true, gameStatus:Statut, colPlayed:Colonne, rowPlayed:Ligne}).


%%%%%%%%%%%%%%%
%%  Utilitaires

% statutJeu/1(+Colonne,+Ligne,+CouleurJCourant,-Statut)
% Unifie à Statut l'état du jeu,
% avec comme dernier coup joué : un jeton en Colonne Ligne par CouleurJCourant
% Status s'unifie à "win" si le coup a amené à une victoire.
statutJeu(Colonne,Ligne,CouleurJCourant, 'win') :-
    gagne(Colonne,Ligne,CouleurJCourant).
% Status s'unifie à "draw" si la partie se termine sur une égalité,
statutJeu(_,_,_, 'draw') :-
    not(coupPossible).
% Status s'unifie à "continue" si la partie n'est pas terminée (dans ce cas le joueur courant est changé),
statutJeu(_,_,_, 'continue') :-
    changerJoueur.

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
