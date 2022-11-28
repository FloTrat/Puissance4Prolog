%%%%%%%%%%%% eval.pl %%%%%%%%%%%%
% Différentes fonctions d'évaluation pour le Puissance 4, toutes basées sur des heuristiques différentes.

:- module(eval, [evalJeu/5, evalTest1/2]).

%%%%%%%%%%%%%%%%
%% Inclusions %%
%%%%%%%%%%%%%%%%

:- use_module(util).
:- use_module(jeu).
:- use_module(ia).
:- use_module(miniMax).

:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats publics %%
%%%%%%%%%%%%%%%%%%%%%%%

% evalJeu/5(+JoueurCourant, +AutreJoueur, +X, +Y, -Score)
% Évalue la situation courante pour le joueur JoueurCourant étant donné que le dernier coup joué fut joué en (X,Y). Le score est pondéré par les différentes pondérations données en entrée (par assert) à evalJeu. Le score est ensuite perturbé par une valeur aléatoire, permettant de casser le caractère déterministe de l'IA.
% Score s'unifie avec le score évalué pour la position courante.
evalJeu(JoueurCourant,AutreJoueur,X,Y,Score) :-
	%write("X: "),write(X),write(" Y: "),write(Y),write(" "),
	assert(ennemiTest(AutreJoueur)),
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	poidsPuissance3(PoidsPuissance3), poidsPosition(PoidsPosition), poidsDensite(PoidsDensite), poidsAdjacence(PoidsAdjacence), poidsTest(PoidsTest), poidsConf(PoidsConf), pertubations(Pertubations),
	evalPosition(JoueurCourant,Score1,PoidsPosition),
	evalPuissances3(JoueurCourant,AutreJoueur,Score2,PoidsPuissance3),
	densite(JoueurCourant,Score3,PoidsDensite),
	evalAdjacence(X,Y,JoueurCourant,Score4, PoidsAdjacence),
	evalTest(JoueurCourant,AutreJoueur,Score5,PoidsTest),
	evalConf(JoueurCourant,AutreJoueur,Score6,PoidsConf),
	%write("Position: "),write(Score1),write(" Puissance3: "),write(Score2),write(" Densite: "),write(Score3),write(" Adjacence: "),write(Score4),write(" Test: "),write(Score5),write(" Conf: "),write(Score6), nl,
	retract(ennemiTest(AutreJoueur)),
	random_between(-2,2,Perturbation),
	Score is Score1 * PoidsPosition
			+ Score2 * PoidsPuissance3
			+ Score3 * PoidsDensite
			+ Score4 * PoidsAdjacence
			+ Score5 * PoidsTest
			+ Score6 * PoidsConf
			+ Perturbation * Pertubations.


%%%%%%%%%%%%%%%%%%%%%%
%% Prédicats privés %%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%		HEURISTIQUE DE POTENTIEL PAR JETON DU JOUEUR         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% evalTest/4(+JoueurCourant,+AutreJoueur,-Score,+PoidsTest)
% Évalue en cherchant les jetons qui peuvent potentiellement faire gagner.
% ScoreFinal s'unifie au score de la grille.
evalTest(_,_,0,0) :- !.
evalTest(JoueurCourant,AutreJoueur,ScoreFinal,_) :-
	findall(S,evalJetonsTest(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalJetonsTest(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant - ScoreAutre.
	%,write(ScoreCourant), write(" "), write(ScoreAutre), write(" "), write(ScoreFinal), nl.


% evalJetonsTest/2(+JoueurCourant,-ScoreTotalJetons)
% Donne un score en fonction des jetons placés par JoueurCourant
evalJetonsTest(Joueur,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseTest(X,Y,Joueur),
	evalLigne(X,Y,Joueur,ScoreLigne),
	evalCol(X,Y,Joueur,ScoreCol),
	evalDiag1(X,Y,Joueur,ScoreDiag1),
	evalDiag2(X,Y,Joueur,ScoreDiag2),
	%write(ScoreLigne), write(" "), write(ScoreCol), write(" "), write(ScoreDiag1), write(" "), write(ScoreDiag2), write(" "), nl,
	%max_list([ScoreLigne,ScoreCol,ScoreDiag1,ScoreDiag2], ScoreCase).
	sum([ScoreLigne,ScoreCol,ScoreDiag1,ScoreDiag2],ScoreCase).

%% ligne
% evalLigne/4(+Colonne,+Ligne,+JoueurCourant,-ScoreJeton)
% Donne un score au jeton par rapport au potentiel de gagner en faisant une ligne avec
evalLigne(X,Y,J,Score) :-
	decr(X,X1),incr(X,X2),
	nombreCasesAvantEnnemi(X2,Y,J,1,0,3,DroiteV),
	nombreCasesAvantEnnemi(X1,Y,J,-1,0,3,GaucheV),
	nombreJetonsJoueurAdj(X2,Y,J,1,0,3,DroiteJ),
	nombreJetonsJoueurAdj(X1,Y,J,-1,0,3,GaucheJ),
	DroiteV+GaucheV+1 >= 4,
	NCaseJoueur is DroiteJ+GaucheJ+1,
	pow(NCaseJoueur,3,Score), !.
evalLigne(_,_,_,0).

%% colonne
% evalCol/4(+Colonne,+Ligne,+JoueurCourant,-ScoreJeton)
% Donne un score au jeton par rapport au potentiel de gagner en faisant une colonne avec
evalCol(X,Y,J,Score) :-
	decr(Y,Y1),
	nombreJetonsJoueurAdj(X,Y1,J,0,-1,3,BasJ),
	NCaseJoueur is BasJ + 1,
	nbLignes(NBLIGNES),
	NBLIGNES-Y+NCaseJoueur >= 4,
	pow(NCaseJoueur,3,Score), !.
evalCol(_,_,_,0).

%% diag1 (descendante)
% evalDiag1/4(+Colonne,+Ligne,+JoueurCourant,-ScoreJeton)
% Donne un score au jeton par rapport au potentiel de gagner en faisant une diagonale descendante avec
evalDiag1(X,Y,J,Score) :-
	decr(X,X1),incr(X,X2),
	decr(Y,Y1),incr(Y,Y2),
	nombreCasesAvantEnnemi(X2,Y1,J,1,-1,3,DroiteV),
	nombreCasesAvantEnnemi(X1,Y2,J,-1,1,3,GaucheV),
	nombreJetonsJoueurAdj(X2,Y1,J,1,-1,3,DroiteJ),
	nombreJetonsJoueurAdj(X1,Y2,J,-1,1,3,GaucheJ),
	DroiteV+GaucheV+1 >= 4,
	NCaseJoueur is DroiteJ+GaucheJ+1,
	pow(NCaseJoueur,3,Score), !.
evalDiag1(_,_,_,0).

%% diag2 (montante)
% evalDiag2/4(+Colonne,+Ligne,+JoueurCourant,-ScoreJeton)
% Donne un score au jeton par rapport au potentiel de gagner en faisant une diagonale montante avec
evalDiag2(X,Y,J,Score) :-
	decr(X,X1),incr(X,X2),
	decr(Y,Y1),incr(Y,Y2),
	nombreCasesAvantEnnemi(X2,Y2,J,1,1,3,DroiteV),
	nombreCasesAvantEnnemi(X1,Y1,J,-1,-1,3,GaucheV),
	nombreJetonsJoueurAdj(X2,Y2,J,1,1,3,DroiteJ),
	nombreJetonsJoueurAdj(X1,Y1,J,-1,-1,3,GaucheJ),
	DroiteV+GaucheV+1 >= 4,
	NCaseJoueur is DroiteJ+GaucheJ+1,
	pow(NCaseJoueur,3,Score), !.
evalDiag2(_,_,_,0).

% nombreJetonsJoueurAdj/7(+X,+Y,+Joueur,+DX,+DY,+TailleConf,-NombreCases)
% NombreCases s'unifie avec le nombre de jetons adjacents placés par le Joueur dans la direction (DX,DY),
% à partir de la position (X,Y) et jusqu'à une distance max de TailleConf
nombreJetonsJoueurAdj(_,_,_,_,_,0,0) :- !.
nombreJetonsJoueurAdj(X,Y,Joueur,_,_,_,0) :-
	not(caseTest(X,Y,Joueur)),!.
nombreJetonsJoueurAdj(X,Y,Joueur,DX,DY,TailleConf,NombreCases) :-
	X1 is X + DX,
	Y1 is Y + DY,
	decr(TailleConf,T1),
	nombreJetonsJoueurAdj(X1,Y1,Joueur,DX,DY,T1,N1),
	incr(N1,NombreCases).%,write(X),write(Y),write(Joueur).

% nombreCasesAvantEnnemi/7(+X,+Y,+Joueur,+DX,+DY,+TailleConf,-NombreCases)
% NombreCases s'unifie avec le nombre de cases dans la direction (DX,DY),
% à partir de la position (X,Y) et jusqu'à une distance max de TailleConf
% avant de tomber sur un jeton ennemi
nombreCasesAvantEnnemi(_,_,_,_,_,0,0) :- !.
nombreCasesAvantEnnemi(X,_,_,_,_,_,0) :- 
	nbColonnes(NBCOLONNES),
	X > NBCOLONNES,!.
nombreCasesAvantEnnemi(0,_,_,_,_,_,0) :- !.
nombreCasesAvantEnnemi(_,Y,_,_,_,_,0) :- 
	nbLignes(NBLIGNES),
	Y > NBLIGNES,!.
nombreCasesAvantEnnemi(_,0,_,_,_,_,0) :- !.
nombreCasesAvantEnnemi(X,Y,_,_,_,_,0) :-
	ennemiTest(Ennemi),caseTest(X,Y,Ennemi),!.
nombreCasesAvantEnnemi(X,Y,Joueur,DX,DY,TailleConf,NombreCases) :-
	X1 is X + DX,
	Y1 is Y + DY,
	decr(TailleConf,T1),
	nombreCasesAvantEnnemi(X1,Y1,Joueur,DX,DY,T1,N1),
	incr(N1,NombreCases).%,write(X),write(Y),write(Joueur).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE CONFIGURATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% evalConf/4(+JoueurCourant,+AutreJoueur,-Score)
% Évalue en parcourant tous les alignement de 4 cases (en ligne/colonne/diagonales)
% ScoreFinal s'unifie au score de la grille.
evalConf(JoueurCourant,AutreJoueur,ScoreFinal,PoidsConf) :-
	PoidsConf>0,
	findall(S,evalConfLigne(JoueurCourant,S),ScoresCourantLigne), sum(ScoresCourantLigne,ScoreCourantLigne),
	findall(S,evalConfCol(JoueurCourant,S),ScoresCourantCol), sum(ScoresCourantCol,ScoreCourantCol),
	findall(S,evalConfDiag1(JoueurCourant,S),ScoresCourantDiag1), sum(ScoresCourantDiag1,ScoreCourantDiag1),
	findall(S,evalConfDiag2(JoueurCourant,S),ScoresCourantDiag2), sum(ScoresCourantDiag2,ScoreCourantDiag2),
	ScoreCourant is ScoreCourantLigne + ScoreCourantCol + ScoreCourantDiag1 + ScoreCourantDiag2,
	findall(S,evalConfLigne(AutreJoueur,S),ScoresAutreLigne), sum(ScoresAutreLigne,ScoreAutreLigne),
	findall(S,evalConfCol(AutreJoueur,S),ScoresAutreCol), sum(ScoresAutreCol,ScoreAutreCol),
	findall(S,evalConfDiag1(AutreJoueur,S),ScoresAutreDiag1), sum(ScoresAutreDiag1,ScoreAutreDiag1),
	findall(S,evalConfDiag2(AutreJoueur,S),ScoresAutreDiag2), sum(ScoresAutreDiag2,ScoreAutreDiag2),
	ScoreAutre is ScoreAutreLigne + ScoreAutreCol + ScoreAutreDiag1 + ScoreAutreDiag2,
	ScoreFinal is ScoreCourant - ScoreAutre
	.%,write(ScoreCourant), write(" "), write(ScoreAutre), write(" "), write(ScoreFinal), nl.
evalConf(_,_,0,_).

%% ligne
% evalConfLigne/2(+Joueur,-ScoreLignes)
% Donne un score à chaque alignement en ligne
evalConfLigne(Joueur,Score) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	Xmax is NBCOLONNES-3,
	between(1,Xmax,X), between(1,NBLIGNES,Y),
	%write(" X: "), write(X), write(" Y: "), write(Y),
	nombreJetonsJoueurConf(X,Y,Joueur,1,0,4,NombreCases),
	pow(5,NombreCases,Score).%, write(" "), write(Score).

%% colonne
% evalConfCol/2(+Joueur,-ScoreLignes)
% Donne un score à chaque alignement en colonne
evalConfCol(Joueur,Score) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	Ymax is NBLIGNES-3,
	between(1,NBCOLONNES,X), between(1,Ymax,Y),
	%write(" X: "), write(X), write(" Y: "), write(Y),
	nombreJetonsJoueurConf(X,Y,Joueur,0,1,4,NombreCases),
	pow(5,NombreCases,Score).%, write(" "), write(Score).

%% diag2 (descendante)
% evalConfDiag1/2(+Joueur,-ScoreLignes)
% Donne un score à chaque alignement en diagonale descendante
evalConfDiag1(Joueur,Score) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	Xmax is NBCOLONNES-3,
	between(1,Xmax,X), between(4,NBLIGNES,Y),
	%write(" X: "), write(X), write(" Y: "), write(Y),
	nombreJetonsJoueurConf(X,Y,Joueur,1,-1,4,NombreCases),
	pow(5,NombreCases,Score).%, write(" "), write(Score).

%% diag2 (montante)
% evalConfDiag2/2(+Joueur,-ScoreLignes)
% Donne un score à chaque alignement en diagonale montante
evalConfDiag2(Joueur,Score) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	Xmax is NBCOLONNES-3,
	Ymax is NBLIGNES-3,
	between(1,Xmax,X), between(1,Ymax,Y),
	%write(" X: "), write(X), write(" Y: "), write(Y),
	nombreJetonsJoueurConf(X,Y,Joueur,1,1,4,NombreCases),
	pow(5,NombreCases,Score).% write(" "), write(Score).

% nombreJetonsJoueurConf/7(+X,+Y,+Joueur,+DX,+DY,+TailleConf,-NombreCases)
% NombreCases s'unifie avec le nombre de jetons placés par le Joueur dans la direction (DX,DY),
% à partir de la position (X,Y) et jusqu'à une distance max de TailleConf
% (il peut y avoir des cases vides entre plusieurs jetons placés par le Joueur)
% Renvoie faux si il y a une jeton adverse dans l'alignement (X,Y)->(X+TailleConf*DX,Y+TailleConf*DY)

%nombreJetonsJoueurConf(_,_,Joueur,_,_,0,1) :- ennemiTest(Joueur),!.
nombreJetonsJoueurConf(_,_,_,_,_,0,0) :- !.
nombreJetonsJoueurConf(X,Y,Joueur,DX,DY,TailleConf,NombreCases) :-
	caseTest(X,Y,Joueur), !,
	X1 is X + DX,
	Y1 is Y + DY,
	decr(TailleConf,T1),
	nombreJetonsJoueurConf(X1,Y1,Joueur,DX,DY,T1,N1),
	incr(N1,NombreCases).%,write(X),write(Y),write(Joueur).
nombreJetonsJoueurConf(X,Y,Joueur,DX,DY,TailleConf,NombreCases) :-
	caseVideTest(X,Y), !,
	X1 is X + DX,
	Y1 is Y + DY,
	decr(TailleConf,T1),
	nombreJetonsJoueurConf(X1,Y1,Joueur,DX,DY,T1,NombreCases).
nombreJetonsJoueurConf(X,Y,_,_,_,_,_) :-
	ennemiTest(Ennemi),caseTest(X,Y,Ennemi), !, false.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fin de modification du code source %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR ADJACENCE                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evalAdjacence/5(+X,+Y,+Joueur,-Note,+PoidsAdjacence)
% Donne une note d'autant plus forte qu'un pion est entouré de pions amis.
% Note s'unifie au score de la position.

evalAdjacence(X,Y,Joueur,Note,PoidsAdjacence) :-
	PoidsAdjacence>0,
	aggregate_all(count,caseAdjacente(X,Y,Joueur,_,_),N),
	decr(N,N1),
	pow(N1,2,Note).
evalAdjacence(_,_,_,0,_).

caseAdjacente(X,Y,Joueur,Xadj,Yadj) :-
	incr(X,X2), decr(X,X1),
	incr(Y,Y2), decr(Y,Y1),
	between(X1,X2,Xadj), between(Y1,Y2,Yadj),
	caseTest(Xadj,Yadj,Joueur).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PAR DENSITE DE PION                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% densite/3(+Joueur,-Note,+PoidsDensite)
% Donne une note d'autant plus élevée que les pions sont groupés.
% Note s'unifie au score de la position.
densite(J,Note,PoidsDensite) :- PoidsDensite>0, Z is 1, calculNbPoints(J,Z,Note).
densite(_,0,_).
calculNbPoints(_,Z,Note) :- Z>6, Note is 0.
calculNbPoints(J,Z,Note) :- nbPointsZone(J,Z,N), incr(Z,ZP), calculNbPoints(J,ZP,NP), Note is N+NP.
nbPointsZone(J,Z,NbPoints) :- nbPionsZone(J,Z,N), pow(N,2,NbPoints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nbPionsZone/3(+Joueur,+Zone,-NbPions)
% Donne le nombre de pions contenu dans une zone.
% NbPions s'unifie au nombre de pions contenu dans une zone.
nbPionsZone(J,Z,NbPions) :-
	aggregate_all(count,caseTestZone(Z,J,_,_),NbPions).

caseTestZone(Zone,Joueur,X,Y) :- caseTest(X,Y,Joueur), zone(Zone,X,Y).
zone(1,X,Y) :- X =<3, Y =< 3.
zone(2,X,Y) :- X = 4, Y =< 3.
zone(3,X,Y) :- X > 4, Y =< 3.
zone(4,X,Y) :- X > 4, Y > 3.
zone(5,X,Y) :- X = 4, Y > 3.
zone(6,X,Y) :- X =<3, Y > 3.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE POSITION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% evalPosition/3(+Courant,-Score,+PoidsPosition)
% Évalue en privilégiant les positions centrales en fonction de la pondération.
% Score s'unifie à une valeur entre -400 et 400.
evalPosition(Courant,Score,PoidsPosition) :-
	PoidsPosition>0,
	assert(nbCasesPleines(0)),
	findall(S, evalCases(Courant,S), Scores),
	sum(Scores, ScoreTot),
	nbCasesPleines(NbCasesPleinesFinal),
	retract(nbCasesPleines(NbCasesPleinesFinal)),
	Score is ScoreTot / (NbCasesPleinesFinal+1).
evalPosition(_,0,_).

evalCases(Courant,ScoreCase) :-
	caseTest(X,Y,_),
	nbCasesPleines(NbCasesPleines),
	retract(nbCasesPleines(NbCasesPleines)),
	incr(NbCasesPleines,NbCasesPleinesF),
	assert(nbCasesPleines(NbCasesPleinesF)),
	evalCase(X,Y,Courant,ScoreCase).

% renvoie un score entre -400 et 400
evalCase(X,Y,Courant,ScoreCase) :-
	nbColonnes(NBCOLONNES),
	nbLignes(NBLIGNES),
	ponderationJ(X, Y, Courant, PonderationJoueur),
	CentreX is NBCOLONNES // 2 + 1,
	CentreY is NBLIGNES // 2 + 1,
	Dx is X - CentreX,
	Dy is Y - CentreY,
	abs(Dx,AbsX),
	abs(Dy,OrdY),
	ScoreCase is ( 200/(AbsX+1) + 200/(OrdY+1) )*PonderationJoueur.


ponderationJ(X,Y, Courant,1) :-
	caseTest(X,Y,Courant), !.
ponderationJ(X,Y,_,-1) :-
	ennemiTest(J),
	caseTest(X,Y,J), !.
ponderationJ(_,_,_,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			HEURISTIQUE PUISSANCE3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% evalPuissances3/3(+JoueurCourant,+AutreJoueur,-Score)
% Évalue en cherchant les positions faisant gagner.
% ScoreFinal s'unifie au score de la position.
evalPuissances3(JoueurCourant,AutreJoueur,ScoreFinal,PoidsPuissance3) :-
	PoidsPuissance3>0,
	findall(S,evalCasesVides(JoueurCourant,S),ScoresCourant), sum(ScoresCourant,ScoreCourant),
	findall(S,evalCasesVides(AutreJoueur,S),ScoresAutre), sum(ScoresAutre,ScoreAutre),
	ScoreFinal is ScoreCourant - ScoreAutre.
evalPuissances3(_,_,0,_).

evalCasesVides(Joueur,ScoreCase) :-
	nbColonnes(NBCOLONNES), nbLignes(NBLIGNES),
	between(1,NBCOLONNES,X), between(1,NBLIGNES,Y),
	caseTest(X,Y,Joueur),
	incr(X,X1),
	decr(X,X2),
	incr(Y,Y1),
	decr(Y,Y2),
	caseVideTest(X1,Y1),
	caseVideTest(X2,Y1),
	caseTest(X2,Y2,_),
	caseTest(X1,Y2,_),
	(gagneTestDirect(X1,Y1,Joueur) -> ScoreCase1=100 ; ScoreCase1=0), % (If -> Then ; Else)
	(gagneTestDirect(X2,Y1,Joueur) -> ScoreCase2=100 ; ScoreCase2=0),
	ScoreCase is ScoreCase1+ScoreCase2.

%%%%% gagneTestDirect %%%%%


gagneTestDirect(X,Y,J) :-
	gagneTestDirectLigne(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag1(X,Y,J).
gagneTestDirect(X,Y,J) :-
	gagneTestDirectDiag2(X,Y,J).


%%% En ligne %%%

gagneTestDirectLigne(X,Y,J) :-
	decr(X,X1),
	gaucheVerif(X1,Y,J,Rg),
	incr(X,X2),
	droiteVerif(X2,Y,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.


gaucheVerif(X,Y,J,Rg):-
	gauche(X,Y,J,0,Rg).
gauche(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gauche(X,Y,J,R,Rg) :-
	decr(X,X1),
	incr(R,R1),
	gauche(X1,Y,J,R1,Rg).

droiteVerif(X,Y,J,Rg):-
	droite(X,Y,J,0,Rg).
droite(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droite(X,Y,J,R,Rg) :-
	incr(X,X1),
	incr(R,R1),
	droite(X1,Y,J,R1,Rg).

%%% En diagonale \ %%%

gagneTestDirectDiag1(X,Y,J) :-
	decr(X,X1),
	incr(Y,Y1),
	gaucheHautVerif(X1,Y1,J,Rg),
	incr(X,X2),
	decr(Y,Y2),
	droiteBasVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheHautVerif(X,Y,J,Rg):-
	gaucheHaut(X,Y,J,0,Rg).
gaucheHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheHaut(X1,Y1,J,R1,Rg).

droiteBasVerif(X,Y,J,Rg):-
	droiteBas(X,Y,J,0,Rg).
droiteBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteBas(X1,Y1,J,R1,Rg).

%%% En diagonale / %%%

gagneTestDirectDiag2(X,Y,J) :-
	decr(X,X1),
	decr(Y,Y1),
	gaucheBasVerif(X1,Y1,J,Rg),
	incr(X,X2),
	incr(Y,Y2),
	droiteHautVerif(X2,Y2,J,Rd),
	!,
	Rf is Rg+Rd,
	Rf>2.

gaucheBasVerif(X,Y,J,Rg) :-
	gaucheBas(X,Y,J,0,Rg).
gaucheBas(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
gaucheBas(X,Y,J,R,Rg) :-
	decr(Y,Y1),
	decr(X,X1),
	incr(R,R1),
	gaucheBas(X1,Y1,J,R1,Rg).

droiteHautVerif(X,Y,J,Rg) :-
	droiteHaut(X,Y,J,0,Rg).
droiteHaut(X,Y,J,R,R) :-
	not(caseTest(X,Y,J)). %Jusqu'à la case non J
droiteHaut(X,Y,J,R,Rg) :-
	incr(Y,Y1),
	incr(X,X1),
	incr(R,R1),
	droiteHaut(X1,Y1,J,R1,Rg).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%% caseVideTest %%%%%
% caseVideTest(+X,+Y)
% vrai si la case X,Y est vide
caseVideTest(X,Y) :- nonvar(X),nonvar(Y),not(caseTest(X,Y,_)).


%%%% Utilisé pour les tests unitaires

evalTest1(1,-3).
evalTest1(2,-4).
evalTest1(3,5).
evalTest1(4,10).
evalTest1(5,9).
evalTest1(6,-5).
evalTest1(7,8).
