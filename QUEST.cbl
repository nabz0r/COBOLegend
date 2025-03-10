      *****************************************************************
      * QUEST.CBL - Système de quêtes de COBOLegend
      *
      * Ce module gère les quêtes, objectifs et progression de l'histoire
      * du jeu.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUEST-SYSTEM.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Structure des quêtes
      *-----------------------------------------------------------------
       01 QUEST-TABLE.
          05 MAX-QUESTS             PIC 9(2)   VALUE 10.
          05 QUESTS OCCURS 10 TIMES INDEXED BY QUEST-IDX.
             10 QUEST-ID            PIC 9(3).
             10 QUEST-NAME          PIC X(30).
             10 QUEST-DESC          PIC X(255).
             10 QUEST-STATUS        PIC X(1).
                88 QUEST-INACTIVE   VALUE 'I'.
                88 QUEST-ACTIVE     VALUE 'A'.
                88 QUEST-COMPLETED  VALUE 'C'.
                88 QUEST-FAILED     VALUE 'F'.
             10 QUEST-TYPE          PIC X(1).
                88 QUEST-MAIN       VALUE 'M'.
                88 QUEST-SIDE       VALUE 'S'.
             10 QUEST-OBJECTIVES    OCCURS 5 TIMES.
                15 OBJECTIVE-DESC   PIC X(50).
                15 OBJECTIVE-STATUS PIC X(1).
                   88 OBJ-INCOMPLETE VALUE 'I'.
                   88 OBJ-COMPLETE   VALUE 'C'.
                15 OBJECTIVE-TARGET PIC 9(3).
                15 OBJECTIVE-PROGRESS PIC 9(3).
             10 QUEST-REWARD-EXP    PIC 9(5).
             10 QUEST-REWARD-GOLD   PIC 9(5).
             10 QUEST-REWARD-ITEM   PIC X(20).
      
       01 QUEST-COUNT               PIC 9(2)   VALUE 0.
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation des quêtes
      *-----------------------------------------------------------------
       INITIALIZE-QUESTS.
           MOVE 0 TO QUEST-COUNT
           PERFORM ADD-MAIN-QUEST
           PERFORM ADD-SIDE-QUEST-1
           .
      
      *-----------------------------------------------------------------
      * Ajout de la quête principale
      *-----------------------------------------------------------------
       ADD-MAIN-QUEST.
           ADD 1 TO QUEST-COUNT
           MOVE QUEST-COUNT TO QUEST-ID(QUEST-COUNT)
           MOVE "Explorer la cité futuriste" 
                TO QUEST-NAME(QUEST-COUNT)
           MOVE "La mystérieuse cité futuriste au loin semble abriter "
                "des secrets sur l'ancien monde. Explorez-la pour "
                "découvrir ses mystères." 
                TO QUEST-DESC(QUEST-COUNT)
           MOVE "A" TO QUEST-STATUS(QUEST-COUNT)
           MOVE "M" TO QUEST-TYPE(QUEST-COUNT)
      
           MOVE "Trouver l'entrée de la cité" 
                TO OBJECTIVE-DESC(QUEST-COUNT, 1)
           MOVE "I" TO OBJECTIVE-STATUS(QUEST-COUNT, 1)
           MOVE 1 TO OBJECTIVE-TARGET(QUEST-COUNT, 1)
           MOVE 0 TO OBJECTIVE-PROGRESS(QUEST-COUNT, 1)
      
           MOVE "Parler au chef des gardiens" 
                TO OBJECTIVE-DESC(QUEST-COUNT, 2)
           MOVE "I" TO OBJECTIVE-STATUS(QUEST-COUNT, 2)
           MOVE 1 TO OBJECTIVE-TARGET(QUEST-COUNT, 2)
           MOVE 0 TO OBJECTIVE-PROGRESS(QUEST-COUNT, 2)
      
           MOVE "Accéder au coeur de la cité" 
                TO OBJECTIVE-DESC(QUEST-COUNT, 3)
           MOVE "I" TO OBJECTIVE-STATUS(QUEST-COUNT, 3)
           MOVE 1 TO OBJECTIVE-TARGET(QUEST-COUNT, 3)
           MOVE 0 TO OBJECTIVE-PROGRESS(QUEST-COUNT, 3)
      
           MOVE 500 TO QUEST-REWARD-EXP(QUEST-COUNT)
           MOVE 200 TO QUEST-REWARD-GOLD(QUEST-COUNT)
           MOVE "Clé d'accès mainframe" 
                TO QUEST-REWARD-ITEM(QUEST-COUNT)
           .
      
      *-----------------------------------------------------------------
      * Ajout d'une quête secondaire
      *-----------------------------------------------------------------
       ADD-SIDE-QUEST-1.
           ADD 1 TO QUEST-COUNT
           MOVE QUEST-COUNT TO QUEST-ID(QUEST-COUNT)
           MOVE "Collecter des fragments de code" 
                TO QUEST-NAME(QUEST-COUNT)
           MOVE "Des fragments de code ancien sont dispersés dans "
                "toute la région. Collectez-les pour déverrouiller "
                "des fonctionnalités perdues." 
                TO QUEST-DESC(QUEST-COUNT)
           MOVE "A" TO QUEST-STATUS(QUEST-COUNT)
           MOVE "S" TO QUEST-TYPE(QUEST-COUNT)
      
           MOVE "Collecter 5 fragments de code" 
                TO OBJECTIVE-DESC(QUEST-COUNT, 1)
           MOVE "I" TO OBJECTIVE-STATUS(QUEST-COUNT, 1)
           MOVE 5 TO OBJECTIVE-TARGET(QUEST-COUNT, 1)
           MOVE 0 TO OBJECTIVE-PROGRESS(QUEST-COUNT, 1)
      
           MOVE 100 TO QUEST-REWARD-EXP(QUEST-COUNT)
           MOVE 50 TO QUEST-REWARD-GOLD(QUEST-COUNT)
           MOVE "Compilateur antique" 
                TO QUEST-REWARD-ITEM(QUEST-COUNT)
           .
      
      *-----------------------------------------------------------------
      * Mise à jour d'un objectif de quête
      *-----------------------------------------------------------------
       UPDATE-QUEST-OBJECTIVE.
           *> Paramètres: QUEST-ID-TO-UPDATE, OBJECTIVE-NUM, PROGRESS-VALUE
           PERFORM VARYING QUEST-IDX FROM 1 BY 1 
                   UNTIL QUEST-IDX > QUEST-COUNT
               IF QUEST-ID(QUEST-IDX) = QUEST-ID-TO-UPDATE
                   ADD PROGRESS-VALUE TO 
                       OBJECTIVE-PROGRESS(QUEST-IDX, OBJECTIVE-NUM)
      
                   IF OBJECTIVE-PROGRESS(QUEST-IDX, OBJECTIVE-NUM) >= 
                      OBJECTIVE-TARGET(QUEST-IDX, OBJECTIVE-NUM)
                       MOVE OBJECTIVE-TARGET(QUEST-IDX, OBJECTIVE-NUM) TO 
                            OBJECTIVE-PROGRESS(QUEST-IDX, OBJECTIVE-NUM)
                       MOVE "C" TO 
                            OBJECTIVE-STATUS(QUEST-IDX, OBJECTIVE-NUM)
      
                       DISPLAY "Objectif complété: " 
                          OBJECTIVE-DESC(QUEST-IDX, OBJECTIVE-NUM)
                   END-IF
      
                   PERFORM CHECK-QUEST-COMPLETION
                   EXIT PERFORM
               END-IF
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Vérification de la complétion d'une quête
      *-----------------------------------------------------------------
       CHECK-QUEST-COMPLETION.
           MOVE "Y" TO ALL-OBJECTIVES-COMPLETED
      
           PERFORM VARYING OBJ-IDX FROM 1 BY 1 UNTIL OBJ-IDX > 5
               IF OBJECTIVE-DESC(QUEST-IDX, OBJ-IDX) NOT = SPACES AND
                  OBJECTIVE-STATUS(QUEST-IDX, OBJ-IDX) = "I"
                   MOVE "N" TO ALL-OBJECTIVES-COMPLETED
                   EXIT PERFORM
               END-IF
           END-PERFORM
      
           IF ALL-OBJECTIVES-COMPLETED = "Y" AND 
              QUEST-STATUS(QUEST-IDX) = "A"
               MOVE "C" TO QUEST-STATUS(QUEST-IDX)
               PERFORM COMPLETE-QUEST
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Traitement de la complétion d'une quête
      *-----------------------------------------------------------------
       COMPLETE-QUEST.
           DISPLAY SPACE
           DISPLAY "*** QUÊTE COMPLÉTÉE: " QUEST-NAME(QUEST-IDX) " ***"
           DISPLAY "Récompenses:"
           DISPLAY "- " QUEST-REWARD-EXP(QUEST-IDX) " points d'expérience"
           DISPLAY "- " QUEST-REWARD-GOLD(QUEST-IDX) " pièces d'or"
           IF QUEST-REWARD-ITEM(QUEST-IDX) NOT = SPACES
               DISPLAY "- Objet: " QUEST-REWARD-ITEM(QUEST-IDX)
           END-IF
      
           MOVE QUEST-REWARD-EXP(QUEST-IDX) TO EXPERIENCE-GAINED
           PERFORM ADD-EXPERIENCE
           ADD QUEST-REWARD-GOLD(QUEST-IDX) TO PLAYER-GOLD
      
           *> Ajout de l'objet à l'inventaire serait géré ici
           .
      
      *-----------------------------------------------------------------
      * Affichage du journal de quêtes
      *-----------------------------------------------------------------
       DISPLAY-QUEST-LOG.
           DISPLAY SPACE
           DISPLAY "=== JOURNAL DE QUÊTES ==="
           DISPLAY SPACE
      
           PERFORM VARYING QUEST-IDX FROM 1 BY 1 
                   UNTIL QUEST-IDX > QUEST-COUNT
               IF QUEST-STATUS(QUEST-IDX) = "A" OR 
                  QUEST-STATUS(QUEST-IDX) = "C"
      
                   IF QUEST-TYPE(QUEST-IDX) = "M"
                       DISPLAY "[PRINCIPALE] " WITH NO ADVANCING
                   ELSE
                       DISPLAY "[SECONDAIRE] " WITH NO ADVANCING
                   END-IF
      
                   IF QUEST-STATUS(QUEST-IDX) = "C"
                       DISPLAY "[TERMINÉE] " WITH NO ADVANCING
                   END-IF
      
                   DISPLAY QUEST-NAME(QUEST-IDX)
                   DISPLAY "  " QUEST-DESC(QUEST-IDX)
                   DISPLAY SPACE
      
                   PERFORM VARYING OBJ-IDX FROM 1 BY 1 UNTIL OBJ-IDX > 5
                       IF OBJECTIVE-DESC(QUEST-IDX, OBJ-IDX) NOT = SPACES
                           IF OBJECTIVE-STATUS(QUEST-IDX, OBJ-IDX) = "C"
                               DISPLAY "  [X] " WITH NO ADVANCING
                           ELSE
                               DISPLAY "  [ ] " WITH NO ADVANCING
                           END-IF
      
                           DISPLAY OBJECTIVE-DESC(QUEST-IDX, OBJ-IDX) 
                               WITH NO ADVANCING
      
                           IF OBJECTIVE-TARGET(QUEST-IDX, OBJ-IDX) > 1
                               DISPLAY " (" 
                                   OBJECTIVE-PROGRESS(QUEST-IDX, OBJ-IDX) 
                                   "/" 
                                   OBJECTIVE-TARGET(QUEST-IDX, OBJ-IDX) 
                                   ")"
                           ELSE
                               DISPLAY SPACE
                           END-IF
                       END-IF
                   END-PERFORM
                   DISPLAY SPACE
               END-IF
           END-PERFORM
           .
      
       END PROGRAM QUEST-SYSTEM.
