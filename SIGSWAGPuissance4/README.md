# PrologPuissance4
Projet Prolog - Jeu du puissance 4   

## Service web  
Pour lancer une partie, il faut exécuter le fichier `websimulate.pl`, puis écrire le prédicat `"initAction."`.
Pour exécuter le fichier `websimulate.pl`:
```
$ swipl websimulate.pl

?- initAction.
```

Il faudra penser à écrire un `"."` à chaque fin de prédicat/commande entré dans le terminal !

## Les sources
Les fichiers sources sont séparés en deux parties : les prédicats "publics" (exportés par le module) et les prédicats "privés". Tandis que nous avons essayés de converser les prédicats publics très homogènes pour des raisons d'interfaçage et de partage du travail, les prédicats privés sont plus organisés selon le bon vouloir de chacun.

## Modification du code de l'ancien projet
Les modifications que nous avons apportées au code de l'ancien projet sont marquées par l'annotation commentée `"Modification du code source"` au début et par une ligne de `"%"` en fin de code ajouté. Les modifications sont présentes un peu partout dans le code d'origine.
