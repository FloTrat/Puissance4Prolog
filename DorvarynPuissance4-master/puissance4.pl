max(X,Y,Y) :- Y>X, !.
max(X,Y,X). 

% Fonction qui permet d'ajouter un élément en fin de liste
ajoutFin(X,[],[X]).
ajoutFin(X,[Y|L1],[Y|L2]):- ajoutFin(X,L1,L2).

finListe([], _).
finListe(L, E):- last(L,E).

% Fonction qui renvoie une sous-liste à partir d'une liste L
/* Paramètres : S sous-liste, L liste */
prefix(P,L):-append(P,_,L).
sublist(S,L):-prefix(S,L).
sublist(S,[_|T]):-sublist(S,T).

% Fonction qui retourne la longueur d'une liste
/* Paramètres : L liste, N longueur de la liste */
longueur([],0).
longueur([_|L],N):- longueur(L,N1),
					N is N1+1.

% Fonction qui renvoie le nième élément d'une liste 
/* Paramètres : N index de l'élement qu'on veut récupérer, L liste, X élément retourné */
nthElem(N, L, []):- longueur(L, N1), N1 < N.
nthElem(N, L, X):- nth1(N, L, X).				

% Fonction qui enregistre un coup joué dans la grille
/* Paramètres : N numéro de la colonne dans laquelle J joue, G grille, J joueur, G' nouvelle grille */		
enregistrerCoup(1, [L|G], x, _, I):- longueur(L,N), N >= 6, write('Coup Invalide\n'), jouerCoupX(I).
enregistrerCoup(1, [L|G], o, _, I):- longueur(L,N), N >= 6, write('Coup Invalide\n'), jouerCoupO(I).
enregistrerCoup(1, [L|G], J, F, I):- longueur(L,N), N < 6, ajoutFin(J,L,M), F=[M|G].
enregistrerCoup(N, [L|G], x, _, I):- N > 7, write('Coup Invalide\n'), jouerCoupX(I).
enregistrerCoup(N, [L|G], o, _, I):- N > 7, write('Coup Invalide\n'), jouerCoupO(I).
enregistrerCoup(N, [T|X], J, [T|G], I):- 	N > 0,
										N1 is N-1,
										enregistrerCoup(N1, X, J, G, I).
					
enregistrerCoupJoueur(1, [L|G], x, _, I):- longueur(L,N), N >= 6, write('Coup Invalide\n'), jouerCoupJoueur(I).
enregistrerCoupJoueur(1, [L|G], J, F, I):- longueur(L,N), N < 6, ajoutFin(J,L,M), F=[M|G].
enregistrerCoupJoueur(N, [L|G], x, _, I):- N > 7, write('Coup Invalide\n'), jouerCoupJoueur(I).	
enregistrerCoupJoueur(N, [T|X], J, [T|G], I):- 	N > 0,
										N1 is N-1,
										enregistrerCoupJoueur(N1, X, J, G, I).

enregistrerCoupIA(1, [L|G], J, F, I):- longueur(L,N), N < 6, ajoutFin(J,L,M), F=[M|G].
enregistrerCoupIA(N, [T|X], J, [T|G], I):- 	N > 0,
										N1 is N-1,
										enregistrerCoupIA(N1, X, J, G, I).
% Condition de victoire verticale : 4 jetons les uns après les autres sur une même colonne
/* Paramètres : G grille, J joueur */										
finJeuVert([L|_],J):- sublist([J,J,J,J], L),!.
finJeuVert([_|G],J):- finJeuVert(G,J).

% Condition de victoire horizontale : 4 jetons les uns après les autres sur une même ligne
/* Paramètres : N numéro de la ligne à partir duquel on traite, G grille, J joueur */
finJeuHor(N, G, J):- maplist(nthElem(N), G, L), 
					 sublist([J,J,J,J],L),!.
finJeuHor(N, G, J):- N > 0,
					 N1 is N-1,
					 finJeuHor(N1, G, J).

finJeuHor(G,J):- finJeuHor(6, G, J).				 

uneFinDiag(G,D,J,0):- sublist([J,J,J,J],D).
uneFinDiag(G,D,J,N):- N > 0,
					  maplist(nthElem(N), G, L),
					  nthElem(N,L,E),
					  N1 is N-1,
					  uneFinDiag(G,[E|D],J,N1).

uneFinDiag(G,J):- uneFinDiag(G,[],J,6).

autreFinDiag(G,D,J,0):- sublist([J,J,J,J],D).
autreFinDiag(G,D,J,N):- N > 0,
					    maplist(nthElem(N), G, L),
						N2 is 7-N,
						nthElem(N2,L,E),
					    N1 is N-1,
					    autreFinDiag(G,[E|D],J,N1).

autreFinDiag(G,J):- autreFinDiag(G,[],J,6).


finJeuDiag(G,N,X,J):- autreFinDiag(X,J),!.
finJeuDiag(G,N,X,J):- uneFinDiag(X,J),!.
finJeuDiag(G,N,X,J):- N < 7,
					  maplist(nthElem(N), G, L),
					  N1 is N+1,
					  finJeuDiag(G,N1,[L|X],J).

finJeuDiag(G,J):- finJeuDiag(G,1,[],J).

% Définition et test des conditions de fin de jeu
/* Paramètres : G grille, J joueur */
finJeu(G, J):- finJeuVert(G,x), J=x.
finJeu(G, J):- finJeuVert(G,o), J=o.
finJeu(G, J):- finJeuHor(G,x), J=x.
finJeu(G, J):- finJeuHor(G,o), J=o.
finJeu(G, J):- finJeuDiag(G,x), J=x.
finJeu(G, J):- finJeuDiag(G,o), J=o.

% Affichage du gagnant
/* Paramètres : J joueur */
gagnant(J):-write('Le Joueur '), write(J), write(' a gagné !').


/* Paramètres : G grille*/
jouerCoupX(G):-finJeu(G,J), gagnant(J),!.
jouerCoupO(G):-finJeu(G,J), gagnant(J),!.
jouerCoupX(G):- write('Joueur x, entrez un numéro de colonne : '),
				read(N), enregistrerCoup(N,G, x, X, G),
				afficherGrille(X),
				write('\n'),
				jouerCoupO(X).
jouerCoupO(G):- write('Joueur o, entrez un numéro de colonne : '),
				read(N), enregistrerCoup(N,G, o, X, G),
				afficherGrille(X),
				write('\n'),
				jouerCoupX(X).

% Lancement du jeu : grille de départ de 6*7 (vide). C'est le joueur 'o' qui commence, suivi par x, jusqu'à ce que l'un des deux gagne [ou GRILLE PLEINE]
jouer:- jouerCoupO([[],[],[],[],[],[],[]]).

%Un coup gagant est un coup qui mene à un état de jeu ou le joueur est vainqueur
coupGagnant(C,G,J):- enregistrerCoupIA(1,G,J,N,G), finJeu(N,J), C=1.
coupGagnant(C,G,J):- enregistrerCoupIA(2,G,J,N,G), finJeu(N,J), C=2.
coupGagnant(C,G,J):- enregistrerCoupIA(3,G,J,N,G), finJeu(N,J), C=3.
coupGagnant(C,G,J):- enregistrerCoupIA(4,G,J,N,G), finJeu(N,J), C=4.
coupGagnant(C,G,J):- enregistrerCoupIA(5,G,J,N,G), finJeu(N,J), C=5.
coupGagnant(C,G,J):- enregistrerCoupIA(6,G,J,N,G), finJeu(N,J), C=6.
coupGagnant(C,G,J):- enregistrerCoupIA(7,G,J,N,G), finJeu(N,J), C=7.

%Un coup perdant est un coup qui permet à l'adversaire de gagner
coupPerdantIA(1,G):- enregistrerCoupIA(1,G,o,N,G), coupGagnant(D,N,x).
coupPerdantIA(2,G):- enregistrerCoupIA(2,G,o,N,G), coupGagnant(D,N,x).
coupPerdantIA(3,G):- enregistrerCoupIA(3,G,o,N,G), coupGagnant(D,N,x).
coupPerdantIA(4,G):- enregistrerCoupIA(4,G,o,N,G), coupGagnant(D,N,x).
coupPerdantIA(5,G):- enregistrerCoupIA(5,G,o,N,G), coupGagnant(D,N,x).
coupPerdantIA(6,G):- enregistrerCoupIA(6,G,o,N,G), coupGagnant(D,N,x).
coupPerdantIA(7,G):- enregistrerCoupIA(7,G,o,N,G), coupGagnant(D,N,x).

jouerCoupJoueur(G):-finJeu(G,J), gagnant(J),!.
jouerIA(G):-finJeu(G,J), gagnant(J),!.

%Si un coup permet de gagner il faut le jouer.
jouerIA(G):-   coupGagnant(C,G,o), enregistrerCoupIA(C,G,o,X,G),
			   afficherGrille(X),
			   write('\n'),
			   jouerCoupJoueur(X).

%Si un coup permet a l'adversaire de gagner on se défend(coup défensif).
jouerIA(G):-   coupGagnant(C,G,x), enregistrerCoupIA(C,G,o,X,G),
			   afficherGrille(X),
			   write('\n'),
			   jouerCoupJoueur(X).

jouerIA(0, G):- write('Pas de coup trouvé').


jouerIA(C, G):- enregistrerCoupIA(C,G,o,X,G),
			    afficherGrille(X),
			    write('\n'),
			    jouerCoupJoueur(X).

espaceRestant(1, [L|G], E, L):- longueur(L,N2), N3 is 6-N2, E=N3.
espaceRestant(N, [T|X], E, L):- N > 0,
								N1 is N-1,
								espaceRestant(N1, X, E, L).
									
%Si on a pas de coup immédiat on fait un coup au centre ou au plus près possible pour une victoire possible en verticale.
jouerIA(G):- espaceRestant(4,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(4,G)), jouerIA(4,G).
jouerIA(G):- espaceRestant(5,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(5,G)), jouerIA(5,G).
jouerIA(G):- espaceRestant(3,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(3,G)), jouerIA(3,G).
jouerIA(G):- espaceRestant(6,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(6,G)), jouerIA(6,G).
jouerIA(G):- espaceRestant(2,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(2,G)), jouerIA(2,G).
jouerIA(G):- espaceRestant(7,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(7,G)), jouerIA(7,G).
jouerIA(G):- espaceRestant(1,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(1,G)), jouerIA(1,G).

%Sinon jouer au plus près du centre quand même.
jouerIA(G):- jouerIA(4,G),not(coupPerdantIA(4,G)).
jouerIA(G):- jouerIA(5,G),not(coupPerdantIA(5,G)).
jouerIA(G):- jouerIA(3,G),not(coupPerdantIA(3,G)).
jouerIA(G):- jouerIA(6,G),not(coupPerdantIA(6,G)).
jouerIA(G):- jouerIA(2,G),not(coupPerdantIA(2,G)).
jouerIA(G):- jouerIA(7,G),not(coupPerdantIA(7,G)).
jouerIA(G):- jouerIA(1,G),not(coupPerdantIA(1,G)).

%Déblocage de situation
jouerIA(G):- jouerIA(4,G).
jouerIA(G):- jouerIA(5,G).
jouerIA(G):- jouerIA(3,G).
jouerIA(G):- jouerIA(6,G).
jouerIA(G):- jouerIA(2,G).
jouerIA(G):- jouerIA(7,G).
jouerIA(G):- jouerIA(1,G).
jouerIA(G):- jouerIA(0,G).

jouerCoupJoueur(G):- write('Joueur x, entrez un numéro de colonne : '),
				read(N), enregistrerCoupJoueur(N,G, x, X, G),
				afficherGrille(X),
				write('\n'),
				jouerIA(X).

lancerIA:- jouerIA([[],[],[],[],[],[],[]]).

enregistrerCoupArbre(1, [L|G], J, [[J|L]|G]):- longueur(L,N), N < 6.
enregistrerCoupArbre(N, [T|X], J, [T|G]):- 	N > 0,
										N1 is N-1,
										enregistrerCoupArbre(N1, X, J, G).
% Evaluation de la grille de jeu
/* Paramètres : G grille, J joueur */
evalVert([], _, P, X):- X=P, write(fini).										
evalVert([L|G],J, P, X):- 	sublist([J,J,J,J], L),
							evalVert(G, J, P, 4, X).
evalVert([L|G],J, P, X):- 	sublist([J,J,J], L),
							evalVert(G, J, P, 3, X).
evalVert([L|G],J, P, X):- 	sublist([J,J], L),
							evalVert(G, J, P, 2, X).
evalVert([L|G],J, P, X):- evalVert(G, J, P, 1, X).
evalVert(G,J, P1, P2, X):- 	max(P1, P2, P),
							evalVert(G, J, P, X).
evalVert(G, J, X):- evalVert(G,J, 0, 1, X).

/* Paramètres : N numéro de la ligne à partir duquel on traite, G grille, J joueur */
evalHor(_,[],J,P):- write(fini).
evalHor(N, G, J, P):- maplist(nthElem(N), G, L), 
					 sublist([J,J,J,J],L),
					 evalHor(N, G, J, P, 4).
evalHor(N, G, J, P):- maplist(nthElem(N), G, L), 
					 sublist([J,J,J],L),
					 evalHor(N, G, J, P, 3).
evalHor(N, G, J, P):- maplist(nthElem(N), G, L), 
					 sublist([J,J],L),
					 evalHor(N, G, J, P, 2).
evalHor(N, G, J, P):- maplist(nthElem(N), G, L), 
					 sublist([J],L),
					 evalHor(N, G, J, P, 1).
evalHor(N, G, J, P1, P2):- N > 0,
					 N1 is N-1,
					 write(toto),
					 max(P1, P2, P),
					 evalHor(N1, G, J, P),
					 write(P).
evalHor(G,J,P):- evalHor(6, G, J, 0, 1).

evalGrille(G, J, X) :- evalHor(G,J,P1),
					evalVert(G, J, P2),	
					max(P1,P2, X).								
										
/* Paramètres : G grille, J joueur, P profondeur, A arbre obtenu */
tracerArbre(G, J, 0, A).
tracerArbre(G, J, P, A):- P > 0,
					      P1 is P-1,
						  tracerBranche(G, J, P1, A, 7).

tracerBranche(G, J, P, A, 1).						  
tracerBranche(G, x, P, A, N):- N > 0,
							   N1 is N-1,
							   enregistrerCoupArbre(N, G, x, X), 
							   tracerArbre(X, o, P, A),
							   tracerBranche(G, x, P, A, N1).			
tracerBranche(G, o, P, A, N):- N > 0,
							   N1 is N-1,
							   enregistrerCoupArbre(N, G, o, X), 
							   tracerArbre(X, x, P, A),
							   tracerBranche(G, o, P, A, N1).
afficherGrille(_,0).							   
afficherGrille(G, N):-	 N > 0,
						N1 is N-1,
						maplist(nthElem(N), G, L),
						afficherListe(L),
						write('\n'),
						afficherGrille(G, N1).

afficherGrille(G):- afficherGrille(G,6).
 
afficherListe([]):- write('|').
afficherListe([E|L]):-  write('|'), 
						afficherElement(E),
						afficherListe(L).
afficherElement([]):- write(' '),!.
afficherElement(E):- write(E).
