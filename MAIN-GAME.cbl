      *****************************************************************
      * MAIN-GAME.CBL - Programme principal de COBOLegend
      *
      * Ce module contient la boucle de jeu principale et coordonne
      * tous les autres modules du système.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-GAME.
       AUTHOR. CLAUDE.
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
      
      *-----------------------------------------------------------------
      * Variables pour les choix du menu
      *-----------------------------------------------------------------
       01 MENU-CHOICE                PIC 9(1)  VALUE 0.
       01 PLAYER-INPUT               PIC X(20) VALUE SPACES.
      
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
           DISPLAY "apercevez les contours d'une cité futuriste."
           DISPLAY "Que souhaitez-vous faire ?"
           DISPLAY "(N)ord, (S)ud, (E)st, (O)uest, (I)nventaire, "
                   "(C)aractéristiques, (Q)uêtes, (X) Menu"
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