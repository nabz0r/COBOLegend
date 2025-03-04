# COBOLegend

Un RPG textuel développé en COBOL, exploitant la richesse du langage pour créer une aventure rétro-futuriste.

## Présentation

COBOLegend est un jeu de rôle textuel développé entièrement en COBOL, rendant hommage aux systèmes mainframe tout en proposant une expérience de jeu immersive. Ce projet démontre comment un langage historique comme COBOL peut être utilisé pour créer des applications interactives et ludiques.

Le jeu se déroule dans un univers rétro-futuriste nommé MAINFRAME-TERRA, où les humains coexistent avec des technologies informatiques anciennes devenues conscientes. Le joueur incarne un héros du COBOL qui doit explorer ce monde, accomplir des quêtes et découvrir les secrets d'une mystérieuse cité futuriste.

## Structure du projet

- `MAIN-GAME.cbl` - Programme principal et boucle de jeu
- `CHARACTER.cbl` - Gestion des personnages et attributs
- `COMBAT.cbl` - Système de combat au tour par tour
- `WORLD.cbl` - Génération et navigation du monde
- `QUEST.cbl` - Gestion des quêtes et objectifs
- `INVENTORY.cbl` - Système d'inventaire et d'objets
- `UI.cbl` - Interface utilisateur et affichage
- `UTILITIES.cbl` - Fonctions utilitaires diverses
- `Makefile` - Facilite la compilation et l'exécution

## Caractéristiques

- Interface textuelle interactive avec encadrements et formatage stylisé
- Système de combat stratégique au tour par tour
- Progression de personnage avec attributs, compétences et niveaux
- Monde ouvert à explorer avec différents types d'environnements
- Quêtes narratives principales et secondaires
- Système d'inventaire et d'équipement complet
- Rencontres aléatoires basées sur l'environnement
- Utilisation intensive des fonctionnalités modernes de COBOL

## Classes de personnage

- **Programmeur** - Spécialiste de l'attaque, utilise des compétences de codage offensif
- **Analyste** - Équilibré entre intelligence et défense, expert en résolution de problèmes
- **Opérateur** - Robuste et puissant, se spécialise dans la force brute et la survie

## Compilation et exécution

### Prérequis

- GnuCOBOL 2.0 ou supérieur
- Make (pour utiliser le Makefile)

### Compilation

Utilisez le Makefile inclus pour compiler facilement le projet :

```bash
make
```

Ou compilez manuellement avec :

```bash
cobc -x -o cobolegend MAIN-GAME.cbl CHARACTER.cbl COMBAT.cbl WORLD.cbl QUEST.cbl INVENTORY.cbl UI.cbl UTILITIES.cbl
```

### Exécution

Après compilation, lancez le jeu avec :

```bash
make run
```

Ou directement :

```bash
./cobolegend
```

## Commandes de jeu

- **N, S, E, O** - Se déplacer dans les directions cardinales
- **I** - Ouvrir l'inventaire
- **C** - Afficher la fiche de personnage
- **Q** - Journal de quêtes
- **X** - Retourner au menu principal

## Développement futur

- Sauvegarde et chargement de parties
- Système de dialogue avec les PNJ
- Davantage de quêtes et d'environnements
- Système économique et marchands
- Artisanat d'objets

## Licence

Ce projet est sous licence MIT.

## À propos

COBOLegend a été créé pour démontrer les capacités du langage COBOL dans un contexte ludique et pour rendre hommage à l'héritage informatique que ce langage représente. Bien que COBOL soit rarement associé au développement de jeux, ce projet montre qu'avec un peu d'imagination, même les langages les plus orientés entreprise peuvent servir à créer des expériences divertissantes.