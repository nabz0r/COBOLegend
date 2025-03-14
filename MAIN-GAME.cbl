      *****************************************************************
      * MAIN-GAME.CBL - Programme principal de COBOLegend
      *
      * Ce module contient la boucle de jeu principale et coordonne
      * tous les autres modules du système.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-GAME.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      * Variables de contrôle du programme
      *-----------------------------------------------------------------
       01 PROGRAM-STATUS             PIC X(1)  VALUE 'R'.
          88 PROGRAM-RUNNING         VALUE 'R'.
          88 PROGRAM-EXIT            VALUE 'X'.
       
       01 KEYBOARD-STATUS.
          05 KEYBOARD-KEY            PIC 9(3).
          05 KEYBOARD-STATUS-FIELD   PIC X.
      
      *-----------------------------------------------------------------
      * État du jeu
      *-----------------------------------------------------------------
       01 GAME-STATE                 PIC X(1)  VALUE 'M'.
          88 STATE-MAIN-MENU         VALUE 'M'.
          88 STATE-GAMEPLAY          VALUE 'G'.
          88 STATE-COMBAT            VALUE 'C'.
          88 STATE-INVENTORY         VALUE 'I'.
          88 STATE-CHARACTER         VALUE 'H'.
          88 STATE-QUEST-LOG         VALUE 'Q'.
          88 STATE-TIME-TRAVEL       VALUE 'T'.
          88 STATE-DIALOG            VALUE 'D'.
          88 STATE-CRAFTING          VALUE 'J'.
      
      *-----------------------------------------------------------------
      * Variables pour les choix du menu
      *-----------------------------------------------------------------
       01 MENU-CHOICE                PIC 9(1)  VALUE 0.
       01 PLAYER-INPUT               PIC X(20) VALUE SPACES.
      
      *-----------------------------------------------------------------
      * Variables pour le système de dialogue
      *-----------------------------------------------------------------
       01 DIALOG-ID-TO-START         PIC 9(3)  VALUE 0.
       01 NPC-NAME                   PIC X(30) VALUE SPACES.
       01 DIALOG-MODE                PIC X(1)  VALUE "N".
          88 IS-IN-DIALOG            VALUE "Y".
      
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-GAME
           PERFORM DISPLAY-INTRO
      
           PERFORM UNTIL PROGRAM-EXIT
               EVALUATE TRUE
                   WHEN STATE-MAIN-MENU
                       PERFORM HANDLE-MAIN-MENU
                   WHEN STATE-GAMEPLAY
                       PERFORM HANDLE-GAMEPLAY
                   WHEN STATE-COMBAT
                       PERFORM HANDLE-COMBAT
                   WHEN STATE-INVENTORY
                       PERFORM HANDLE-INVENTORY
                   WHEN STATE-CHARACTER
                       PERFORM HANDLE-CHARACTER-SCREEN
                   WHEN STATE-QUEST-LOG
                       PERFORM HANDLE-QUEST-LOG
                   WHEN STATE-TIME-TRAVEL
                       PERFORM HANDLE-TIME-TRAVEL
                   WHEN STATE-DIALOG
                       PERFORM HANDLE-DIALOG
                   WHEN STATE-CRAFTING
                       PERFORM HANDLE-JCL-CRAFTING
               END-EVALUATE
           END-PERFORM
      
           PERFORM CLEANUP-GAME
           STOP RUN.
      
      *-----------------------------------------------------------------
      * Initialisation du jeu
      *-----------------------------------------------------------------
       INITIALIZE-GAME.
           DISPLAY "Initialisation de COBOLegend..."
           DISPLAY "Chargement des données de jeu..."
           DISPLAY "Préparation du monde de jeu..."
           DISPLAY "Initialisation du Terminal Time Travel..."
           DISPLAY "Initialisation du système de dialogue..."
           DISPLAY "Initialisation du système de crafting JCL..."
           DISPLAY "Initialisation terminée."
           .
      
      *-----------------------------------------------------------------
      * Affichage de l'introduction
      *-----------------------------------------------------------------
       DISPLAY-INTRO.
           DISPLAY SPACE
           DISPLAY "***********************************************"
           DISPLAY "*                                             *"
           DISPLAY "*               COBOLegend                    *"
           DISPLAY "*        Une aventure dans le monde           *"
           DISPLAY "*           de MAINFRAME-TERRA                *"
           DISPLAY "*                                             *"
           DISPLAY "***********************************************"
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour continuer..."
           ACCEPT PLAYER-INPUT
           .
      
      *-----------------------------------------------------------------
      * Gestion du menu principal
      *-----------------------------------------------------------------
       HANDLE-MAIN-MENU.
           PERFORM DISPLAY-MAIN-MENU
           ACCEPT MENU-CHOICE
      
           EVALUATE MENU-CHOICE
               WHEN 1
                   MOVE 'G' TO GAME-STATE
               WHEN 2
                   PERFORM DISPLAY-HELP
               WHEN 3
                   MOVE 'X' TO PROGRAM-STATUS
               WHEN OTHER
                   DISPLAY "Choix invalide. Veuillez réessayer."
           END-EVALUATE
           .
      
       DISPLAY-MAIN-MENU.
           DISPLAY SPACE
           DISPLAY "=== MENU PRINCIPAL ==="
           DISPLAY "1. Nouvelle partie"
           DISPLAY "2. Aide"
           DISPLAY "3. Quitter"
           DISPLAY "Votre choix: " WITH NO ADVANCING
           .
      
      *-----------------------------------------------------------------
      * Affichage de l'aide
      *-----------------------------------------------------------------
       DISPLAY-HELP.
           DISPLAY SPACE
           DISPLAY "=== AIDE ==="
           DISPLAY "COBOLegend est un jeu de rôle textuel où vous"
           DISPLAY "explorez le monde de MAINFRAME-TERRA."
           DISPLAY SPACE
           DISPLAY "Commandes disponibles pendant le jeu:"
           DISPLAY "N, S, E, O - Se déplacer dans les directions"
           DISPLAY "I - Ouvrir l'inventaire"
           DISPLAY "C - Afficher la fiche de personnage"
           DISPLAY "Q - Journal de quêtes"
           DISPLAY "T - Accéder au Terminal Time Travel (si disponible)"
           DISPLAY "P - Parler aux personnages à proximité"
           DISPLAY "J - Accéder au système de crafting JCL"
           DISPLAY "X - Retourner au menu principal"
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour continuer..."
           ACCEPT PLAYER-INPUT
           .
      
      *-----------------------------------------------------------------
      * Boucle principale de gameplay
      *-----------------------------------------------------------------
       HANDLE-GAMEPLAY.
           DISPLAY SPACE
           DISPLAY "Vous êtes dans une vaste plaine. Au loin, vous"
           DISPLAY "apercevez les contours d'une cité futuriste. Un"
           DISPLAY "étrange terminal semble briller au nord-est."
           DISPLAY SPACE
           DISPLAY "Une femme en tenue d'archiviste se tient près d'un bâtiment."
           DISPLAY "Un technicien travaille sur des machines à proximité."
           DISPLAY "Un étrange terminal de fabrication est visible à l'ouest."
           DISPLAY SPACE
           DISPLAY "Que souhaitez-vous faire ?"
           DISPLAY "(N)ord, (S)ud, (E)st, (O)uest, (I)nventaire, "
                   "(C)aractéristiques, (Q)uêtes, (T)erminal,"
                   " (P)arler, (J)CL-crafting, (X) Menu"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT PLAYER-INPUT
      
           EVALUATE PLAYER-INPUT
               WHEN "N" WHEN "n"
                   DISPLAY "Vous vous dirigez vers le nord."
               WHEN "S" WHEN "s"
                   DISPLAY "Vous vous dirigez vers le sud."
               WHEN "E" WHEN "e"
                   DISPLAY "Vous vous dirigez vers l'est."
               WHEN "O" WHEN "o"
                   DISPLAY "Vous vous dirigez vers l'ouest."
               WHEN "I" WHEN "i"
                   MOVE 'I' TO GAME-STATE
               WHEN "C" WHEN "c"
                   MOVE 'H' TO GAME-STATE
               WHEN "Q" WHEN "q"
                   MOVE 'Q' TO GAME-STATE
               WHEN "T" WHEN "t"
                   MOVE 'T' TO GAME-STATE
               WHEN "P" WHEN "p"
                   PERFORM SELECT-CHARACTER-TO-TALK
               WHEN "J" WHEN "j"
                   MOVE 'J' TO GAME-STATE
               WHEN "X" WHEN "x"
                   MOVE 'M' TO GAME-STATE
               WHEN OTHER
                   DISPLAY "Commande non reconnue."
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Gestion du combat
      *-----------------------------------------------------------------
       HANDLE-COMBAT.
           DISPLAY "Système de combat - À implémenter"
           MOVE 'G' TO GAME-STATE
           .
      
      *-----------------------------------------------------------------
      * Gestion de l'inventaire
      *-----------------------------------------------------------------
       HANDLE-INVENTORY.
           DISPLAY SPACE
           DISPLAY "=== INVENTAIRE ==="
           DISPLAY "Épée rouillée - ATT+1"
           DISPLAY "Bouclier en bois - DEF+1"
           DISPLAY "Potion de soin x3"
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour revenir au jeu..."
           ACCEPT PLAYER-INPUT
           MOVE 'G' TO GAME-STATE
           .
      
      *-----------------------------------------------------------------
      * Affichage de la fiche de personnage
      *-----------------------------------------------------------------
       HANDLE-CHARACTER-SCREEN.
           DISPLAY SPACE
           DISPLAY "=== PERSONNAGE ==="
           DISPLAY "Nom: Héros du COBOL"
           DISPLAY "Niveau: 1"
           DISPLAY "PV: 20/20"
           DISPLAY "Force: 5"
           DISPLAY "Défense: 3"
           DISPLAY "Intelligence: 4"
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour revenir au jeu..."
           ACCEPT PLAYER-INPUT
           MOVE 'G' TO GAME-STATE
           .
      
      *-----------------------------------------------------------------
      * Affichage du journal de quêtes
      *-----------------------------------------------------------------
       HANDLE-QUEST-LOG.
           DISPLAY SPACE
           DISPLAY "=== JOURNAL DE QUÊTES ==="
           DISPLAY "[!] Quête principale: Explorer la cité futuriste"
           DISPLAY "    - Trouver l'entrée de la cité"
           DISPLAY "    - Parler au chef des gardiens"
           DISPLAY SPACE
           DISPLAY "[ ] Quête secondaire: Collecter 5 fragments de code"
           DISPLAY "    - Fragments trouvés: 0/5"
           DISPLAY SPACE
           DISPLAY "[!] Quête temporelle: Découvrir l'origine de MAINFRAME-TERRA"
           DISPLAY "    - Explorez les différentes époques via le Terminal Time Travel"
           DISPLAY SPACE
           DISPLAY "[ ] Quête d'artisanat: Maîtriser le JCL Crafting"
           DISPLAY "    - Fabriquer 3 objets différents via le système JCL"
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour revenir au jeu..."
           ACCEPT PLAYER-INPUT
           MOVE 'G' TO GAME-STATE
           .
      
      *-----------------------------------------------------------------
      * Gestion du Terminal Time Travel
      *-----------------------------------------------------------------
       HANDLE-TIME-TRAVEL.
           DISPLAY SPACE
           DISPLAY "Vous vous approchez du terminal étrange qui émet"
           DISPLAY "une lueur bleutée. L'interface affiche:"
           DISPLAY SPACE
           DISPLAY "***********************************************"
           DISPLAY "*              CHRONOTERMINAL                 *"
           DISPLAY "*   Portail vers les epoques informatiques   *"
           DISPLAY "***********************************************"
           DISPLAY SPACE
           DISPLAY "Voulez-vous activer le terminal ? (O/N)"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT PLAYER-INPUT
      
           IF PLAYER-INPUT = "O" OR PLAYER-INPUT = "o"
               DISPLAY SPACE
               DISPLAY "Le terminal s'anime..."
               DISPLAY SPACE
               DISPLAY "Veuillez consulter le module Terminal Time Travel"
               DISPLAY "pour explorer les différentes époques informatiques."
               DISPLAY SPACE
               DISPLAY "Note: Cette fonctionnalité est gérée par le module TERMINAL-TIME-TRAVEL.cbl"
               DISPLAY "et peut être compilée séparément avec: make time-travel"
           END-IF
      
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour revenir au jeu..."
           ACCEPT PLAYER-INPUT
           MOVE 'G' TO GAME-STATE
           .
      
      *-----------------------------------------------------------------
      * Sélection d'un personnage pour dialoguer
      *-----------------------------------------------------------------
       SELECT-CHARACTER-TO-TALK.
           DISPLAY SPACE
           DISPLAY "=== PERSONNAGES À PROXIMITÉ ==="
           DISPLAY "1. Archiviste Ada"
           DISPLAY "2. Technicien Turing"
           DISPLAY "3. Gardien Neumann (près de la cité)"
           DISPLAY "4. Retour"
           DISPLAY "Avec qui souhaitez-vous parler? " WITH NO ADVANCING
           ACCEPT MENU-CHOICE
      
           EVALUATE MENU-CHOICE
               WHEN 1
                   MOVE 1 TO DIALOG-ID-TO-START
                   MOVE "Archiviste Ada" TO NPC-NAME
                   MOVE "Y" TO DIALOG-MODE
                   MOVE 'D' TO GAME-STATE
               WHEN 2
                   MOVE 2 TO DIALOG-ID-TO-START
                   MOVE "Technicien Turing" TO NPC-NAME
                   MOVE "Y" TO DIALOG-MODE
                   MOVE 'D' TO GAME-STATE
               WHEN 3
                   MOVE 3 TO DIALOG-ID-TO-START
                   MOVE "Gardien Neumann" TO NPC-NAME
                   MOVE "Y" TO DIALOG-MODE
                   MOVE 'D' TO GAME-STATE
               WHEN 4
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Choix invalide."
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Gestion des dialogues
      *-----------------------------------------------------------------
       HANDLE-DIALOG.
           DISPLAY SPACE
           DISPLAY "Conversation avec " NPC-NAME
           DISPLAY SPACE
           DISPLAY "Note: Ce dialogue est géré par le module DIALOG-MULTIPLEXER.cbl"
           DISPLAY "où les dialogues sont définis avec des options de ramification."
           DISPLAY SPACE
      
           *> Simulation du comportement du module DIALOG-MULTIPLEXER
           EVALUATE DIALOG-ID-TO-START
               WHEN 1
                   DISPLAY "Ada: Bonjour, voyageur. Bienvenue dans la Bibliothèque"
                   DISPLAY "      Centrale de MAINFRAME-TERRA. Je suis Ada, gardienne"
                   DISPLAY "      des connaissances anciennes."
                   DISPLAY SPACE
                   DISPLAY "1. Parlez-moi de cette bibliothèque."
                   DISPLAY "2. Que savez-vous sur la cité futuriste?"
                   DISPLAY "3. Je dois y aller, au revoir."
               WHEN 2
                   DISPLAY "Turing: *bruit de cliquetis* Oh! Vous m'avez surpris."
                   DISPLAY "        Je ne reçois pas souvent de visiteurs ici."
                   DISPLAY "        Je suis Turing, technicien en chef de cette"
                   DISPLAY "        section. Que puis-je faire pour vous?"
                   DISPLAY SPACE
                   DISPLAY "1. Que faites-vous ici?"
                   DISPLAY "2. J'ai trouvé cet étrange composant..."
                   DISPLAY "3. Je ne faisais que passer."
               WHEN 3
                   DISPLAY "Neumann: Halte! Je suis Neumann, gardien de cette entrée."
                   DISPLAY "         Personne ne peut passer sans démontrer sa compréhension"
                   DISPLAY "         du langage ancien. Êtes-vous prêt à relever le défi?"
                   DISPLAY SPACE
                   DISPLAY "1. Je suis prêt. Quel est ce défi?"
                   DISPLAY "2. Je reviendrai quand je serai mieux préparé."
           END-EVALUATE
      
           DISPLAY SPACE
           DISPLAY "Entrez un choix (ou 0 pour terminer le dialogue): " 
                   WITH NO ADVANCING
           ACCEPT MENU-CHOICE
      
           IF MENU-CHOICE = 0
               MOVE "N" TO DIALOG-MODE
               MOVE 'G' TO GAME-STATE
           ELSE
               DISPLAY SPACE
               DISPLAY "Simulation de réponse au choix " MENU-CHOICE "..."
               DISPLAY "Dans l'implémentation complète, le module DIALOG-MULTIPLEXER"
               DISPLAY "traiterait ce choix et poursuivrait la conversation."
               DISPLAY SPACE
               DISPLAY "Appuyez sur ENTRÉE pour terminer le dialogue..."
               ACCEPT PLAYER-INPUT
               MOVE "N" TO DIALOG-MODE
               MOVE 'G' TO GAME-STATE
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Gestion du système de crafting JCL
      *-----------------------------------------------------------------
       HANDLE-JCL-CRAFTING.
           DISPLAY SPACE
           DISPLAY "Vous approchez du terminal de fabrication. Son écran affiche:"
           DISPLAY SPACE
           DISPLAY "***********************************************"
           DISPLAY "*           TERMINAL DE FABRICATION           *"
           DISPLAY "*    Créer des objets avec du code JCL       *"
           DISPLAY "***********************************************"
           DISPLAY SPACE
           DISPLAY "Voulez-vous utiliser le terminal de fabrication ? (O/N)"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT PLAYER-INPUT
      
           IF PLAYER-INPUT = "O" OR PLAYER-INPUT = "o"
               DISPLAY SPACE
               DISPLAY "Le terminal s'active..."
               DISPLAY SPACE
               DISPLAY "Le JCL (Job Control Language) est un langage de contrôle"
               DISPLAY "utilisé dans les environnements mainframe pour définir les"
               DISPLAY "paramètres d'exécution des programmes."
               DISPLAY SPACE
               DISPLAY "En utilisant ce terminal, vous pouvez écrire du code JCL"
               DISPLAY "pour créer différents objets utiles dans votre aventure."
               DISPLAY SPACE
               DISPLAY "Note: Cette fonctionnalité est gérée par le module JCL-CRAFTING.cbl"
               DISPLAY "et peut être compilée séparément."
               DISPLAY SPACE
               DISPLAY "Exemple de code JCL simple:"
               DISPLAY "//COMPILE JOB CLASS=A,MSGCLASS=X"
               DISPLAY "//STEP1   EXEC PGM=IEBGENER"
               DISPLAY "//SYSIN   DD DUMMY"
           END-IF
      
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour revenir au jeu..."
           ACCEPT PLAYER-INPUT
           MOVE 'G' TO GAME-STATE
           .
      
      *-----------------------------------------------------------------
      * Nettoyage final avant sortie
      *-----------------------------------------------------------------
       CLEANUP-GAME.
           DISPLAY SPACE
           DISPLAY "Merci d'avoir joué à COBOLegend!"
           DISPLAY "À bientôt pour de nouvelles aventures..."
           DISPLAY SPACE
           .
      
       END PROGRAM MAIN-GAME.
