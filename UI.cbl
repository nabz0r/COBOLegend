      *****************************************************************
      * UI.CBL - Système d'interface utilisateur de COBOLegend
      *
      * Ce module gère tous les aspects de l'affichage et de l'interface
      * utilisateur du jeu.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UI-SYSTEM.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEYBOARD-STATUS.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Variables d'état du clavier
      *-----------------------------------------------------------------
       01 KEYBOARD-STATUS.
          05 KEYBOARD-KEY            PIC 9(3).
          05 KEYBOARD-STATUS-FIELD   PIC X.
      
      *-----------------------------------------------------------------
      * Constantes d'affichage
      *-----------------------------------------------------------------
       01 SCREEN-WIDTH               PIC 9(2)   VALUE 80.
       01 SCREEN-HEIGHT              PIC 9(2)   VALUE 25.
       01 BOX-X                      PIC 9(2)   VALUE 5.
       01 BOX-Y                      PIC 9(2)   VALUE 3.
       01 BOX-WIDTH                  PIC 9(2)   VALUE 70.
       01 BOX-HEIGHT                 PIC 9(2)   VALUE 20.
      
      *-----------------------------------------------------------------
      * Caractères pour dessiner des cadres
      *-----------------------------------------------------------------
       01 BORDER-CHARS.
          05 CORNER-TL-CHAR          PIC X(1)   VALUE "+".
          05 CORNER-TR-CHAR          PIC X(1)   VALUE "+".
          05 CORNER-BL-CHAR          PIC X(1)   VALUE "+".
          05 CORNER-BR-CHAR          PIC X(1)   VALUE "+".
          05 HORIZONTAL-CHAR         PIC X(1)   VALUE "-".
          05 VERTICAL-CHAR           PIC X(1)   VALUE "|".
      
      *-----------------------------------------------------------------
      * Variables de formatage
      *-----------------------------------------------------------------
       01 TEMP-LINE                  PIC X(80)  VALUE SPACES.
       01 OFFSET-X                   PIC 9(2)   VALUE 0.
       01 OFFSET-Y                   PIC 9(2)   VALUE 0.
       01 LINE-INDEX                 PIC 9(2)   VALUE 1.
       01 CHAR-INDEX                 PIC 9(2)   VALUE 1.
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Effacement de l'écran
      *-----------------------------------------------------------------
       CLEAR-SCREEN.
           DISPLAY SPACE
           PERFORM VARYING LINE-INDEX FROM 1 BY 1 
                   UNTIL LINE-INDEX > SCREEN-HEIGHT
               DISPLAY SPACE
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Dessin d'un cadre
      *-----------------------------------------------------------------
       DRAW-BOX.
           *> Paramètres optionnels: BOX-X BOX-Y BOX-WIDTH BOX-HEIGHT
      
           *> Ligne du haut
           MOVE SPACES TO TEMP-LINE
           STRING CORNER-TL-CHAR DELIMITED BY SIZE
                  INTO TEMP-LINE WITH POINTER 1
      
           PERFORM VARYING CHAR-INDEX FROM 2 BY 1 
                   UNTIL CHAR-INDEX > BOX-WIDTH - 1
               STRING HORIZONTAL-CHAR DELIMITED BY SIZE
                      INTO TEMP-LINE WITH POINTER CHAR-INDEX
           END-PERFORM
      
           STRING CORNER-TR-CHAR DELIMITED BY SIZE
                  INTO TEMP-LINE WITH POINTER CHAR-INDEX
      
           DISPLAY TEMP-LINE
      
           *> Lignes du milieu
           PERFORM VARYING LINE-INDEX FROM 1 BY 1 
                   UNTIL LINE-INDEX > BOX-HEIGHT - 2
               MOVE SPACES TO TEMP-LINE
               STRING VERTICAL-CHAR DELIMITED BY SIZE
                      INTO TEMP-LINE WITH POINTER 1
      
               PERFORM VARYING CHAR-INDEX FROM 2 BY 1 
                       UNTIL CHAR-INDEX > BOX-WIDTH - 1
                   STRING SPACE DELIMITED BY SIZE
                          INTO TEMP-LINE WITH POINTER CHAR-INDEX
               END-PERFORM
      
               STRING VERTICAL-CHAR DELIMITED BY SIZE
                      INTO TEMP-LINE WITH POINTER CHAR-INDEX
      
               DISPLAY TEMP-LINE
           END-PERFORM
      
           *> Ligne du bas
           MOVE SPACES TO TEMP-LINE
           STRING CORNER-BL-CHAR DELIMITED BY SIZE
                  INTO TEMP-LINE WITH POINTER 1
      
           PERFORM VARYING CHAR-INDEX FROM 2 BY 1 
                   UNTIL CHAR-INDEX > BOX-WIDTH - 1
               STRING HORIZONTAL-CHAR DELIMITED BY SIZE
                      INTO TEMP-LINE WITH POINTER CHAR-INDEX
           END-PERFORM
      
           STRING CORNER-BR-CHAR DELIMITED BY SIZE
                  INTO TEMP-LINE WITH POINTER CHAR-INDEX
      
           DISPLAY TEMP-LINE
           .
      
      *-----------------------------------------------------------------
      * Affichage d'un texte centré
      *-----------------------------------------------------------------
       DISPLAY-CENTERED-TEXT.
           *> Paramètre: TEXT-TO-CENTER
      
           COMPUTE OFFSET-X = FUNCTION MAX(
               (SCREEN-WIDTH - FUNCTION LENGTH(TEXT-TO-CENTER)) / 2, 1)
      
           MOVE SPACES TO TEMP-LINE
           STRING TEXT-TO-CENTER DELIMITED BY SIZE
                  INTO TEMP-LINE WITH POINTER OFFSET-X
      
           DISPLAY TEMP-LINE
           .
      
      *-----------------------------------------------------------------
      * Affichage d'une barre de progression
      *-----------------------------------------------------------------
       DISPLAY-PROGRESS-BAR.
           *> Paramètres: CURRENT-VALUE MAX-VALUE BAR-WIDTH
      
           COMPUTE FILLED-CHARS = 
               (CURRENT-VALUE * BAR-WIDTH) / MAX-VALUE
      
           MOVE SPACES TO TEMP-LINE
           STRING "[" DELIMITED BY SIZE
                  INTO TEMP-LINE WITH POINTER 1
      
           PERFORM VARYING CHAR-INDEX FROM 2 BY 1 
                   UNTIL CHAR-INDEX > BAR-WIDTH + 1
               IF CHAR-INDEX - 1 <= FILLED-CHARS
                   STRING "=" DELIMITED BY SIZE
                          INTO TEMP-LINE WITH POINTER CHAR-INDEX
               ELSE
                   STRING " " DELIMITED BY SIZE
                          INTO TEMP-LINE WITH POINTER CHAR-INDEX
               END-IF
           END-PERFORM
      
           STRING "]" DELIMITED BY SIZE
                  INTO TEMP-LINE WITH POINTER CHAR-INDEX
      
           STRING " " CURRENT-VALUE "/" MAX-VALUE DELIMITED BY SIZE
                  INTO TEMP-LINE WITH POINTER CHAR-INDEX + 1
      
           DISPLAY TEMP-LINE
           .
      
      *-----------------------------------------------------------------
      * Affichage du titre du jeu (ASCII Art)
      *-----------------------------------------------------------------
       DISPLAY-GAME-TITLE.
           DISPLAY SPACE
           DISPLAY " .d8888b.   .d88888b.  888888b.    .d88888b.  888      "
           DISPLAY "d88P  Y88b d88P" "Y88b 888  "88b  d88P" "Y88b 888      "
           DISPLAY "888    888 888     888 888  .88P  888     888 888      "
           DISPLAY "888        888     888 8888888K.  888     888 888      "
           DISPLAY "888        888     888 888  "Y88b 888     888 888      "
           DISPLAY "888    888 888     888 888    888 888     888 888      "
           DISPLAY "Y88b  d88P Y88b. .d88P 888   d88P Y88b. .d88P 888      "
           DISPLAY " "Y8888P"   "Y88888P"  8888888P"   "Y88888P"  88888888 "
           DISPLAY "                                                       "
           DISPLAY "            888                  .d8888b.  888          "
           DISPLAY "            888                 d88P  Y88b 888          "
           DISPLAY "            888                 888    888 888          "
           DISPLAY "            888      .d88b.     888        888 .d8888b  "
           DISPLAY "            888     d8P  Y8b    888  88888 888 88K      "
           DISPLAY "            888     88888888    888    888 888 "Y8888b. "
           DISPLAY "            888     Y8b.        Y88b  d88P 888      X88 "
           DISPLAY "            88888888 "Y8888      "Y8888P88 888  88888P' "
           DISPLAY SPACE
           .
      
      *-----------------------------------------------------------------
      * Affichage de l'état du joueur (HUD)
      *-----------------------------------------------------------------
       DISPLAY-PLAYER-STATUS.
           DISPLAY SPACE
           DISPLAY "=== ÉTAT DU JOUEUR ==="
           DISPLAY "Nom: " CHAR-NAME
           DISPLAY "Niveau: " CHAR-LEVEL
           DISPLAY "Classe: " CHAR-CLASS
           DISPLAY "PV: " CHAR-HEALTH-CURRENT "/" CHAR-HEALTH-MAX
      
           MOVE CHAR-HEALTH-CURRENT TO CURRENT-VALUE
           MOVE CHAR-HEALTH-MAX TO MAX-VALUE
           MOVE 20 TO BAR-WIDTH
           PERFORM DISPLAY-PROGRESS-BAR
      
           DISPLAY "PM: " CHAR-MANA-CURRENT "/" CHAR-MANA-MAX
      
           MOVE CHAR-MANA-CURRENT TO CURRENT-VALUE
           MOVE CHAR-MANA-MAX TO MAX-VALUE
           MOVE 20 TO BAR-WIDTH
           PERFORM DISPLAY-PROGRESS-BAR
      
           DISPLAY "EXP: " CHAR-EXPERIENCE "/" CHAR-EXPERIENCE-NEXT
      
           MOVE CHAR-EXPERIENCE TO CURRENT-VALUE
           MOVE CHAR-EXPERIENCE-NEXT TO MAX-VALUE
           MOVE 20 TO BAR-WIDTH
           PERFORM DISPLAY-PROGRESS-BAR
      
           DISPLAY "FOR: " CHAR-STRENGTH "  DEF: " CHAR-DEFENSE 
                   "  INT: " CHAR-INTELLIGENCE "  AGI: " CHAR-AGILITY
           .
      
      *-----------------------------------------------------------------
      * Affichage d'une scène de dialogue
      *-----------------------------------------------------------------
       DISPLAY-DIALOGUE.
           *> Paramètres: SPEAKER-NAME DIALOGUE-TEXT
      
           MOVE 3 TO BOX-Y
           MOVE 15 TO BOX-HEIGHT
           PERFORM DRAW-BOX
      
           DISPLAY SPEAKER-NAME ": "
           DISPLAY DIALOGUE-TEXT
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour continuer..."
           ACCEPT PLAYER-INPUT
           .
      
       END PROGRAM UI-SYSTEM.
