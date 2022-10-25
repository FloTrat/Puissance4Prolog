%%%%%%%%%%%% tests.pl %%%%%%%%%%%%
% Quelques tests unitaires.

:- use_module(jeu).
:- use_module(util).
:- use_module(miniMax).

%%%%% Tests victoire (gagne) %%%%%

t_gagne_colonne :-
	assert(case(1,1,rouge)),
	assert(case(1,2,rouge)),
	assert(case(1,3,rouge)),
	assert(case(1,4,rouge)),
	gagne(1,4,rouge).

t_gagne_ligne :-
	assert(case(1,1,rouge)),
	assert(case(2,1,rouge)),
	assert(case(3,1,rouge)),
	assert(case(4,1,rouge)),
	gagne(4,1,rouge).

t_gagne_diagonale1 :-
	assert(case(1,1,rouge)),
	assert(case(2,2,rouge)),
	assert(case(3,3,rouge)),
	assert(case(4,4,rouge)),
	gagne(4,4,rouge).

t_gagne_diagonale2 :-
	assert(case(4,4,rouge)),
	assert(case(3,3,rouge)),
	assert(case(2,2,rouge)),
	assert(case(1,1,rouge)),
	gagne(1,1,rouge).

%%%%% Tests Minimax %%%%%

t_minimax_prof1 :-
	assert(evaluation(test1)),
	parcoursArbre(rouge,1,R,Value),retract(evaluation(X)),R==4,Value==10.

t_minimax_prof2 :-
	assert(evaluation(test1)),
	parcoursArbre(rouge,2,R,Value),retract(evaluation(X)),R==1,Value==(-5).



%%%%% Tests changer de joueur (changerJoueur) %%%%%

t_changer_joueur1 :-
	assert(joueurCourant(rouge, 1)),
	assert(autreJoueur(jaune, 2)),
	changerJoueur,
	joueurCourant(jaune, 2),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)).

t_changer_joueur2 :-
	assert(joueurCourant(jaune, 3)),
	assert(autreJoueur(rouge, 1)),
	changerJoueur,
	joueurCourant(rouge, 1),
	retractall(joueurCourant(_,_)),
	retractall(autreJoueur(_,_)).

%%%%% Tests coup valide (coupValide) %%%%%

t_coup_valide1 :-
	coupValide(1),
	coupValide(2),
	coupValide(3),
	coupValide(4),
	coupValide(5),
	coupValide(6),
	coupValide(7).

t_coup_invalide1 :-
	not(coupValide(0)).

t_coup_invalide2 :-
	not(coupValide(8)).

t_coup_invalide3 :-
	not(coupValide(-1)).

t_coup_valide2 :-
	assert(case(1,1,rouge)),
	assert(case(1,2,rouge)),
	assert(case(1,3,rouge)),
	assert(case(1,4,rouge)),
	assert(case(1,5,rouge)),
	coupValide(1).

t_coup_invalide4 :-
	assert(case(1,1,rouge)),
	assert(case(1,2,rouge)),
	assert(case(1,3,rouge)),
	assert(case(1,4,rouge)),
	assert(case(1,5,rouge)),
	assert(case(1,5,rouge)),
	assert(case(1,6,rouge)),
	not(coupValide(1)).

%%%%% Tests insérer jeton (insererJeton) %%%%%

t_inserer_jeton1 :-
	insererJeton(1, X, rouge),
	X == 1,
	case(1,1, rouge),
	retractall(case(_,_,_)).

t_inserer_jeton2 :-
	insererJeton(1, _, rouge),
	insererJeton(1, _, rouge),
	insererJeton(2, _, jaune),
	case(1,1, rouge),
	case(1,2, rouge),
	case(2,1, jaune),
	retractall(case(_,_,_)).
