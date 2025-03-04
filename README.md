```
                                                                               
  /$$$$$$   /$$$$$$  /$$$$$$$   /$$$$$$  /$$       /$$$$$$$$  /$$$$$$  /$$$$$$$$  /$$$$$$   /$$$$$$$ 
 /$$__  $$ /$$__  $$| $$__  $$ /$$__  $$| $$      | $$_____/ /$$__  $$| $$_____/ /$$__  $$ /$$_____/
| $$  \__/| $$  \ $$| $$  \ $$| $$  \ $$| $$      | $$      | $$  \__/| $$      | $$  \ $$|  $$$$$$ 
| $$      | $$  | $$| $$$$$$$ | $$  | $$| $$      | $$$$$   | $$ /$$$$| $$$$$   | $$  | $$ \____  $$
| $$      | $$  | $$| $$__  $$| $$  | $$| $$      | $$__/   | $$|_  $$| $$__/   | $$  | $$ /$$__  $$
| $$    $$| $$  | $$| $$  \ $$| $$  | $$| $$      | $$      | $$  \ $$| $$      | $$  | $$|  $$$$$$$
|  $$$$$$/|  $$$$$$/| $$$$$$$/|  $$$$$$/| $$$$$$$$| $$$$$$$$|  $$$$$$/| $$$$$$$$|  $$$$$$/ \_______/
 \_______/ \______/ |_______/  \______/ |________/|________/ \______/ |________/ \______/          
                                                                                                    
```

```
   ___________________________________________________________________________________
  |                                                                                   |
  |     ####      /\                 _______________                                  |
  |    ######    /  \      |\       |  MAINFRAME  |        O                         |
  |   ###  ###  /____\     ||       |_____________|       /|\                        |
  |  ###    ### |    |    _||_                           / | \                       |
  | ########### |____|   /____\      /=\    /=\         __|__                        |
  |                      |    |      |_|    |_|         |   |                        |
  |      MONDE           |____|                         |___|                        |
  |                                                                                   |
  |___________________________________________________________________________________|
          |                                    |                     |
          |                                    |                     |
          V                                    V                     V
    EXPLORATION                           COMBAT                 QUÊTES
```

# 👾 COBOLegend - L'Aventure RPG en COBOL 👾

> *"Dans un monde où chaque IF/THEN/ELSE change le destin, seul un Héros du COBOL peut sauver MAINFRAME-TERRA!"*

## 💻 Qu'est-ce que c'est ? 💻

COBOLegend est un RPG textuel rétro-futuriste qui prouve que même un langage de 1959 peut créer des aventures épiques! 

Imaginez un monde où les mainframes sont des cités vivantes, les bugs sont des monstres tangibles, et les compilateurs sont des artefacts magiques. Vous incarnez un héros du COBOL qui doit naviguer dans cet univers étrange pour découvrir les secrets d'une cité futuriste.

## ⚙️ Composants Techniques ⚙️

```
+------------------+     +------------------+     +------------------+
|   MAIN-GAME.cbl  |<--->|  CHARACTER.cbl  |<--->|    COMBAT.cbl   |
+------------------+     +------------------+     +------------------+
         ^                       ^                       ^
         |                       |                       |
         v                       v                       v
+------------------+     +------------------+     +------------------+
|    WORLD.cbl     |<--->|    QUEST.cbl    |<--->|  INVENTORY.cbl  |
+------------------+     +------------------+     +------------------+
         ^                       ^                       ^
         |                       |                       |
         v                       v                       v
+------------------+     +------------------+     +------------------+
|     UI.cbl       |<--->|  UTILITIES.cbl  |<--->|    Makefile     |
+------------------+     +------------------+     +------------------+
         ^                       ^
         |                       |
         v                       v
+------------------+     +----------------------+
|  EASTER_EGGS.cbl |<--->|TERMINAL-TIME-TRAVEL.cbl|
+------------------+     +----------------------+
```

## 🕹️ Classes de Personnage 🕹️

👨‍💻 **Programmeur** - Maîtrise le code comme arme et peut lancer des "boucles infinies" pour étourdir les ennemis!

👩‍💻 **Analyste** - Expert en débogage qui peut identifier les faiblesses de n'importe quel système hostile!

👷 **Opérateur** - Gardien des mainframes, capable de résister aux crash systèmes les plus violents!

## 🎮 Comment Jouer 🎮

### Compilation

```bash
# Exécutez cet incantation ancestrale pour assembler le jeu:
make

# Ou pour les puristes:
cobc -x -o cobolegend MAIN-GAME.cbl CHARACTER.cbl COMBAT.cbl WORLD.cbl QUEST.cbl INVENTORY.cbl UI.cbl UTILITIES.cbl TERMINAL-TIME-TRAVEL.cbl
```

### Lancement

```bash
# Invoquez le jeu:
make run

# Ou directement:
./cobolegend
```

### Commandes

```
+-----------------------------------+
| COMMANDES MAGIQUES DE NAVIGATION |
+-----------------------------------+
| N, S, E, O - Déplacement         |
| I - Inventaire                   |
| C - Caractéristiques             |
| Q - Quêtes                       |
| X - Menu principal               |
+-----------------------------------+
```

## 🎡 Terminal Time Travel 🎡

Explorez l'histoire de l'informatique grâce au module Terminal Time Travel ! Cette fonctionnalité vous permet de voyager à travers cinq époques distinctes, de l'ère des cartes perforées à l'informatique en nuage moderne.

### Ères Disponibles

1. **Ère des Cartes Perforées (1950-1960)** - Explorez les premières machines à calcul
2. **Ère des Mainframes (1960-1970)** - Découvrez l'âge d'or de COBOL
3. **Ère Microinformatique (1980-1990)** - Témoins de l'essor des ordinateurs personnels
4. **Ère Internet (1990-2000)** - Participez à la révolution du web
5. **Ère Cloud (2010-2020)** - Explorez l'informatique moderne

Chaque époque offre des défis, artefacts et quêtes uniques. Vos actions dans le passé affectent le présent et le futur de MAINFRAME-TERRA !

E
etenez les anomalies temporelles, récupérez des artefacts perdus, et découvrez l'origine véritable de votre monde digital.

## 👽 Ennemis Légendaires 👽

- **Loup Binaire** - Créature sauvage qui attaque par paires
- **Golem de Données** - Monstre massif composé de dossiers corrigds
- **Bug Fatal** - Apparition cauchemardesque qui fait planter les systèmes
- **ABEND Spectre** - Entité qui termine anormalement toute chose qu'elle touche

## 📜 Histoire de MAINFRAME-TERRA 📜

Il y a longtemps, au temps des cartes perforées et des bandes magnétiques, les Anciens Programmeurs ont créé les premiers mainframes. Ces machines, alimentées par le langage mystique du COBOL, ont évolué au fil du temps jusqu'à former leur propre civilisation: MAINFRAME-TERRA.

Aujourd'hui, une mystérieuse cité futuriste menace l'équilibre du monde avec sa technologie avancée. Seul un véritable Héros du COBOL peut découvrir ses secrets et sauver MAINFRAME-TERRA!

## 🚀 Développement Futur 🚀

- **Multiplexeur de Dialogues** - Système de conversation avancé avec les PNJ
- **Expansion VSAM** - Nouvelles zones à explorer
- **JCL Crafting** - Créez vos propres objets grâce au Job Control Language

## 🅰️ À Propos 🅰️

COBOLegend est une preuve que même les langages de programmation les plus anciens peuvent être utilisés pour créer des expériences ludiques. Alors que les jeunes programmeurs courent après les derniers frameworks JavaScript, nous restons fidèles à l'esprit des pionniers de l'informatique. 

Après tout, si COBOL fait encore tourner le monde bancaire, pourquoi ne pourrait-il pas faire tourner votre prochaine aventure?

```
                      FABRIQUÉ AVEC COBOL ET AMOUR
            _____________________________________________________
           /                                                     \
          |    01 PROJET-LICENCE.                                 |
          |       05 LICENCE-TYPE       PIC X(3) VALUE "MIT".     |
          |       05 PERMISSIONS        PIC X(50) VALUE          |
          |          "Utilisez, modifiez et partagez librement". |
          |    01 REMERCIEMENTS         PIC X(24) VALUE          |
          |       "Merci d'avoir joué !".                         |
           \_____________________________________________________/
```

**Remarque**: Aucun mainframe n'a été blessé dans la création de ce jeu.