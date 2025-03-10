      *****************************************************************
      * CHARACTER.CBL - Gestion des personnages dans COBOLegend
      *
      * Ce module gère la création, progression et attributs des
      * personnages du jeu, incluant le personnage joueur et les PNJ.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHARACTER-SYSTEM.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
      
      *-----------------------------------------------------------------
      * Structure de définition d'un personnage
      *-----------------------------------------------------------------
       01 PLAYER-CHARACTER.
          05 CHAR-NAME               PIC X(20).
          05 CHAR-LEVEL              PIC 9(2)   VALUE 1.
          05 CHAR-EXPERIENCE         PIC 9(6)   VALUE 0.
          05 CHAR-EXPERIENCE-NEXT    PIC 9(6)   VALUE 100.
          05 CHAR-CLASS              PIC X(15).
          05 CHAR-STATISTICS.
             10 CHAR-HEALTH-CURRENT  PIC 9(3)   VALUE 20.
             10 CHAR-HEALTH-MAX      PIC 9(3)   VALUE 20.
             10 CHAR-MANA-CURRENT    PIC 9(3)   VALUE 10.
             10 CHAR-MANA-MAX        PIC 9(3)   VALUE 10.
             10 CHAR-STRENGTH        PIC 9(2)   VALUE 5.
             10 CHAR-DEFENSE         PIC 9(2)   VALUE 3.
             10 CHAR-INTELLIGENCE    PIC 9(2)   VALUE 4.
             10 CHAR-AGILITY         PIC 9(2)   VALUE 4.
          05 CHAR-SKILLS.
             10 CHAR-SKILL OCCURS 5 TIMES.
                15 SKILL-NAME        PIC X(20).
                15 SKILL-LEVEL       PIC 9(1)   VALUE 1.
                15 SKILL-TYPE        PIC X(1).
                   88 SKILL-ATTACK   VALUE 'A'.
                   88 SKILL-DEFENSE  VALUE 'D'.
                   88 SKILL-SUPPORT  VALUE 'S'.
                15 SKILL-COST        PIC 9(2)   VALUE 0.
                15 SKILL-POWER       PIC 9(3)   VALUE 0.
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation d'un nouveau personnage
      *-----------------------------------------------------------------
       INITIALIZE-CHARACTER.
           MOVE "Héros du COBOL"     TO CHAR-NAME
           MOVE "Programmeur"        TO CHAR-CLASS
           MOVE 1                    TO CHAR-LEVEL
           MOVE 0                    TO CHAR-EXPERIENCE
           MOVE 100                  TO CHAR-EXPERIENCE-NEXT
           MOVE 20                   TO CHAR-HEALTH-CURRENT
           MOVE 20                   TO CHAR-HEALTH-MAX
           MOVE 10                   TO CHAR-MANA-CURRENT
           MOVE 10                   TO CHAR-MANA-MAX
           MOVE 5                    TO CHAR-STRENGTH
           MOVE 3                    TO CHAR-DEFENSE
           MOVE 4                    TO CHAR-INTELLIGENCE
           MOVE 4                    TO CHAR-AGILITY
      
           MOVE "Frappe de code"     TO SKILL-NAME(1)
           MOVE 'A'                  TO SKILL-TYPE(1)
           MOVE 0                    TO SKILL-COST(1)
           MOVE 5                    TO SKILL-POWER(1)
      
           MOVE "Boucle défensive"   TO SKILL-NAME(2)
           MOVE 'D'                  TO SKILL-TYPE(2)
           MOVE 3                    TO SKILL-COST(2)
           MOVE 8                    TO SKILL-POWER(2)
           .
      
      *-----------------------------------------------------------------
      * Gestion de l'expérience et montée de niveau
      *-----------------------------------------------------------------
       ADD-EXPERIENCE.
           MOVE FUNCTION ADD(CHAR-EXPERIENCE, EXPERIENCE-GAINED) 
                                     TO CHAR-EXPERIENCE
           IF CHAR-EXPERIENCE >= CHAR-EXPERIENCE-NEXT THEN
               PERFORM LEVEL-UP
           END-IF
           .
      
       LEVEL-UP.
           ADD 1                     TO CHAR-LEVEL
           COMPUTE CHAR-EXPERIENCE-NEXT = CHAR-EXPERIENCE-NEXT * 1.5
      
           ADD 5                     TO CHAR-HEALTH-MAX
           ADD 3                     TO CHAR-MANA-MAX
           MOVE CHAR-HEALTH-MAX      TO CHAR-HEALTH-CURRENT
           MOVE CHAR-MANA-MAX        TO CHAR-MANA-CURRENT
      
           EVALUATE CHAR-CLASS
               WHEN "Programmeur"
                   ADD 2             TO CHAR-STRENGTH
                   ADD 1             TO CHAR-DEFENSE
                   ADD 2             TO CHAR-INTELLIGENCE
                   ADD 1             TO CHAR-AGILITY
               WHEN "Analyste"
                   ADD 1             TO CHAR-STRENGTH
                   ADD 1             TO CHAR-DEFENSE
                   ADD 3             TO CHAR-INTELLIGENCE
                   ADD 1             TO CHAR-AGILITY
               WHEN "Opérateur"
                   ADD 3             TO CHAR-STRENGTH
                   ADD 2             TO CHAR-DEFENSE
                   ADD 1             TO CHAR-INTELLIGENCE
                   ADD 0             TO CHAR-AGILITY
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Gestion des statistiques en combat
      *-----------------------------------------------------------------
       CALCULATE-ATTACK-POWER.
           COMPUTE ATTACK-POWER = 
               CHAR-STRENGTH + (CHAR-LEVEL * 2) + WEAPON-BONUS
           .
      
       CALCULATE-DEFENSE-VALUE.
           COMPUTE DEFENSE-VALUE = 
               CHAR-DEFENSE + (CHAR-LEVEL / 2) + ARMOR-BONUS
           .
      
       END PROGRAM CHARACTER-SYSTEM.
