# PrologPuissance4
Projet Prolog - Jeu du puissance 4   

## Service web DÉPRÉCIÉ
Pour lancer le serveur prolog, exécutez le fichier `webserver.pl` et écrivez le prédicat `server(8000).` (ou `start.`). Puis accédez à `http://localhost:8000/game` pour jouer !

## IHM 
Pour pouvoir jouer au puissance 4, il faut compiler le fichier ./SIGSWAGPuissance4/websimulate.pl puis lancer le prédicat initAction.

## Tests et benchmarking 
Pour pouvoir lancer les tests entre IAs ou le benchmarking, il faut compiler le fichier ./SIGSWAGPuissance4/testIAs.pl puis lancer le prédicat runTest(nbItérations, IA1, IA2). ou le prédicat benchmark(NbItérations, IA1, IA2).

## Les sources
Les fichiers sources sont séparés en deux parties : les prédicats "publics" (exportés par le module) et les prédicats "privés". Tandis que nous avons essayés de converser les prédicats publics très homogènes pour des raisons d'interfaçage et de partage du travail, les prédicats privés sont plus organisés selon le bon vouloir de chacun.
