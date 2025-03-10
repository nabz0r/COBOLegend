      *****************************************************************
      * UTILITIES.CBL - Fonctions utilitaires pour COBOLegend
      *
      * Ce module contient diverses fonctions utilitaires utilisées
      * par d'autres modules du jeu.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTILITIES.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Variables pour la génération de nombres aléatoires
      *-----------------------------------------------------------------
       01 RANDOM-SEED               PIC 9(9)  VALUE 0.
       01 RANDOM-VALUE              PIC 9V9(5).
       01 RANDOM-RANGE              PIC 9(5)  VALUE 0.
       01 RANDOM-RESULT             PIC 9(5)  VALUE 0.
       01 TEMP-RANDOM               PIC 9(9).
      
      *-----------------------------------------------------------------
      * Variables pour le traitement de chaînes
      *-----------------------------------------------------------------
       01 STRING-LENGTH             PIC 9(3)  VALUE 0.
       01 CHAR-INDEX                PIC 9(3)  VALUE 1.
       01 RESULT-STRING             PIC X(255) VALUE SPACES.
       01 TEMP-CHAR                 PIC X(1)  VALUE SPACE.
      
      *-----------------------------------------------------------------
      * Variables pour les autres fonctions
      *-----------------------------------------------------------------
       01 CONTAINS-RESULT           PIC X(1)  VALUE "N".
       01 MIN-VALUE                 PIC 9(5)  VALUE 0.
       01 MAX-VALUE                 PIC 9(5)  VALUE 0.
       01 FILLED-CHARS              PIC 9(3)  VALUE 0.
       01 ALL-OBJECTIVES-COMPLETED  PIC X(1)  VALUE "Y".
       01 QUEST-ID-TO-UPDATE        PIC 9(3)  VALUE 0.
       01 OBJECTIVE-NUM             PIC 9(1)  VALUE 0.
       01 PROGRESS-VALUE            PIC 9(3)  VALUE 0.
       01 OBJ-IDX                   PIC 9(1)  VALUE 0.
       01 EQUIP-IDX                 PIC 9(1)  VALUE 0.
       01 EQUIPMENT-SLOT            PIC 9(1)  VALUE 0.
       01 MOVE-IDX                  PIC 9(2)  VALUE 0.
       01 TARGET-IDX                PIC 9(2)  VALUE 0.
       01 EXPERIENCE-GAINED         PIC 9(5)  VALUE 0.
       01 WAS-ADDED                 PIC X(1)  VALUE "N".
       01 ITEM-IDX-TO-EQUIP         PIC 9(2)  VALUE 0.
       01 ITEM-IDX-TO-USE           PIC 9(2)  VALUE 0.
       01 ITEM-IDX-TO-REMOVE        PIC 9(2)  VALUE 0.
       01 WEAPON-BONUS              PIC 9(3)  VALUE 0.
       01 ARMOR-BONUS               PIC 9(3)  VALUE 0.
       01 ITEM-ID-TO-ADD            PIC 9(3)  VALUE 0.
       01 ITEM-NAME-TO-ADD          PIC X(20) VALUE SPACES.
       01 ITEM-DESC-TO-ADD          PIC X(100) VALUE SPACES.
       01 ITEM-TYPE-TO-ADD          PIC X(1)  VALUE "M".
       01 ITEM-VALUE-TO-ADD         PIC 9(5)  VALUE 0.
       01 ITEM-QUANTITY-TO-ADD      PIC 9(2)  VALUE 1.
       01 INPUT-STRING              PIC X(255) VALUE SPACES.
       01 HAYSTACK                  PIC X(255) VALUE SPACES.
       01 NEEDLE                    PIC X(50)  VALUE SPACES.
       01 TEXT-TO-CENTER            PIC X(80)  VALUE SPACES.
       01 SPEAKER-NAME              PIC X(30)  VALUE SPACES.
       01 DIALOGUE-TEXT             PIC X(255) VALUE SPACES.
       01 CURRENT-VALUE             PIC 9(5)  VALUE 0.
       01 BAR-WIDTH                 PIC 9(2)  VALUE 20.
            
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation du générateur de nombres aléatoires
      *-----------------------------------------------------------------
       INITIALIZE-RANDOM-GENERATOR.
           ACCEPT RANDOM-SEED FROM DAY-OF-WEEK
           MULTIPLY RANDOM-SEED BY 10000 GIVING RANDOM-SEED
           ACCEPT TEMP-RANDOM FROM TIME
           ADD TEMP-RANDOM TO RANDOM-SEED
           .
      
      *-----------------------------------------------------------------
      * Génération d'un nombre aléatoire dans une plage définie
      *-----------------------------------------------------------------
       GENERATE-RANDOM-NUMBER.
           *> Paramètres: MIN-VALUE MAX-VALUE
           *> Retourne: RANDOM-RESULT
      
           COMPUTE RANDOM-VALUE = FUNCTION RANDOM
           COMPUTE RANDOM-RANGE = MAX-VALUE - MIN-VALUE + 1
           COMPUTE RANDOM-RESULT = 
               MIN-VALUE + (RANDOM-VALUE * RANDOM-RANGE)
           .
      
      *-----------------------------------------------------------------
      * Conversion d'une chaîne en majuscules
      *-----------------------------------------------------------------
       CONVERT-TO-UPPERCASE.
           *> Paramètre: INPUT-STRING
           *> Retourne: RESULT-STRING
      
           MOVE SPACES TO RESULT-STRING
           MOVE FUNCTION LENGTH(INPUT-STRING) TO STRING-LENGTH
      
           PERFORM VARYING CHAR-INDEX FROM 1 BY 1 
                   UNTIL CHAR-INDEX > STRING-LENGTH
               MOVE INPUT-STRING(CHAR-INDEX:1) TO TEMP-CHAR
               MOVE FUNCTION UPPER-CASE(TEMP-CHAR) 
                    TO RESULT-STRING(CHAR-INDEX:1)
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Vérification si une chaîne contient une sous-chaîne
      *-----------------------------------------------------------------
       STRING-CONTAINS.
           *> Paramètres: HAYSTACK NEEDLE
           *> Retourne: CONTAINS-RESULT (Y/N)
      
           MOVE "N" TO CONTAINS-RESULT
           MOVE FUNCTION LENGTH(HAYSTACK) TO STRING-LENGTH
           MOVE FUNCTION LENGTH(NEEDLE) TO NEEDLE-LENGTH
      
           IF NEEDLE-LENGTH > STRING-LENGTH
               EXIT PARAGRAPH
           END-IF
      
           PERFORM VARYING CHAR-INDEX FROM 1 BY 1
                   UNTIL CHAR-INDEX > (STRING-LENGTH - NEEDLE-LENGTH + 1)
                       OR CONTAINS-RESULT = "Y"
               IF HAYSTACK(CHAR-INDEX:NEEDLE-LENGTH) = NEEDLE
                   MOVE "Y" TO CONTAINS-RESULT
               END-IF
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Formatage de nombre avec séparateurs de milliers
      *-----------------------------------------------------------------
       FORMAT-NUMBER-WITH-COMMAS.
           *> Paramètre: NUMBER-TO-FORMAT
           *> Retourne: FORMATTED-NUMBER
      
           MOVE NUMBER-TO-FORMAT TO TEMP-NUMBER
           MOVE SPACES TO FORMATTED-NUMBER
           MOVE 1 TO FORMAT-POS
      
           PERFORM VARYING FORMAT-IDX FROM 12 BY -1
                   UNTIL FORMAT-IDX < 1
               IF MOD(13 - FORMAT-IDX, 3) = 1 AND FORMAT-IDX < 12
                   MOVE "," TO FORMATTED-NUMBER(FORMAT-POS:1)
                   ADD 1 TO FORMAT-POS
               END-IF
      
               MOVE TEMP-NUMBER(FORMAT-IDX:1) TO 
                    FORMATTED-NUMBER(FORMAT-POS:1)
               ADD 1 TO FORMAT-POS
           END-PERFORM
           .
      
       END PROGRAM UTILITIES.
