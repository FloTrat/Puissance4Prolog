# PrologPuissance4
Projet Prolog - Jeu du puissance 4
Le socle du projet est nommé SIGSWAGPuissance4 et réalisé par un groupe d'INSAliens en 2015 : https://github.com/SIGSWAG/PrologPuissance4 à savoir Marc JAVIN et Loïc TOUZARD.


## Jouer à Puissance 4  
Pour lancer une partie, il faut exécuter le fichier `websimulate.pl`, puis écrire le prédicat `"initAction."`.
Pour exécuter le fichier `websimulate.pl`:
```
$ swipl websimulate.pl

?- initAction.
```

Il faudra penser à écrire un `"."` à chaque fin de prédicat/commande entré dans le terminal !

## Tests et benchmarking 
Pour pouvoir lancer les tests entre IAs ou le benchmarking, il faut compiler le fichier ./SIGSWAGPuissance4/testIAs.pl puis lancer le prédicat runTest(nbItérations, IA1, IA2). ou le prédicat benchmark(NbItérations, IA1, IA2).

## Les sources
Les fichiers sources sont séparés en deux parties : les prédicats "publics" (exportés par le module) et les prédicats "privés". Tandis que nous avons essayés de converser les prédicats publics très homogènes pour des raisons d'interfaçage et de partage du travail, les prédicats privés sont plus organisés selon le bon vouloir de chacun.

## Modification du code de l'ancien projet
Les modifications que nous avons apportées au code de l'ancien projet sont marquées par l'annotation commentée `"Modification du code source"` au début et par `"Fin de modification du code source"` en fin de code ajouté. Les modifications sont présentes un peu partout dans le code d'origine, mais nous avons principalement implémenté 2 nouvelles heuristiques (dans le fichier `eval.pl`) et réimplémenté les algorithmes MiniMax avec et sans élagage Alpha/Beta.