      *****************************************************************
      * COMBAT.CBL - Système de combat de COBOLegend
      *
      * Ce module gère toutes les interactions de combat, incluant
      * l'initialisation des combats, les tours de combat, les attaques
      * et la gestion des résultats.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMBAT-SYSTEM.
       AUTHOR. CLAUDE.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Constantes et indicateurs de combat
      *-----------------------------------------------------------------
       01 COMBAT-STATUS              PIC X(1)  VALUE 'A'.
          88 COMBAT-ACTIVE           VALUE 'A'.
          88 COMBAT-PLAYER-VICTORY   VALUE 'V'.
          88 COMBAT-PLAYER-DEFEAT    VALUE 'D'.
          88 COMBAT-ESCAPED          VALUE 'E'.
      
       01 COMBAT-TURN                PIC 9(3)  VALUE 1.
       01 CURRENT-ACTOR              PIC X(1)  VALUE 'P'.
          88 PLAYER-TURN             VALUE 'P'.
          88 ENEMY-TURN              VALUE 'E'.
      
      *-----------------------------------------------------------------
      * Structure de l'ennemi
      *-----------------------------------------------------------------
       01 ENEMY-CHARACTER.
          05 ENEMY-NAME              PIC X(20).
          05 ENEMY-LEVEL             PIC 9(2).
          05 ENEMY-HEALTH-CURRENT    PIC 9(3).
          05 ENEMY-HEALTH-MAX        PIC 9(3).
          05 ENEMY-ATTACK            PIC 9(2).
          05 ENEMY-DEFENSE           PIC 9(2).
          05 ENEMY-EXPERIENCE        PIC 9(4).
      
      *-----------------------------------------------------------------
      * Variables de calcul de combat
      *-----------------------------------------------------------------
       01 ATTACK-POWER               PIC 9(3).
       01 DEFENSE-VALUE              PIC 9(3).
       01 DAMAGE-DEALT               PIC 9(3).
       01 RANDOM-FACTOR              PIC 9V99.
       01 ESCAPE-CHANCE              PIC 9(2).
       01 COMBAT-CHOICE              PIC 9(1).
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation d'un combat
      *-----------------------------------------------------------------
       INITIALIZE-COMBAT.
           MOVE 'A'                  TO COMBAT-STATUS
           MOVE 1                    TO COMBAT-TURN
           MOVE 'P'                  TO CURRENT-ACTOR
      
           DISPLAY SPACE
           DISPLAY "Un ", ENEMY-NAME, " apparaît!"
           DISPLAY "Niveau ", ENEMY-LEVEL
           DISPLAY "PV: ", ENEMY-HEALTH-CURRENT, "/", ENEMY-HEALTH-MAX
           DISPLAY SPACE
           .
      
      *-----------------------------------------------------------------
      * Boucle principale de combat
      *-----------------------------------------------------------------
       COMBAT-LOOP.
           PERFORM UNTIL NOT COMBAT-ACTIVE
               IF PLAYER-TURN
                   PERFORM PLAYER-COMBAT-TURN
               ELSE
                   PERFORM ENEMY-COMBAT-TURN
               END-IF
      
               PERFORM CHECK-COMBAT-STATUS
               PERFORM SWITCH-TURNS
               ADD 1 TO COMBAT-TURN
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Tour de combat du joueur
      *-----------------------------------------------------------------
       PLAYER-COMBAT-TURN.
           DISPLAY "Tour ", COMBAT-TURN
           DISPLAY "Vos PV: ", CHAR-HEALTH-CURRENT, "/", CHAR-HEALTH-MAX
           DISPLAY "PM: ", CHAR-MANA-CURRENT, "/", CHAR-MANA-MAX
           DISPLAY "Ennemi: ", ENEMY-NAME, " PV: ", 
                  ENEMY-HEALTH-CURRENT, "/", ENEMY-HEALTH-MAX
           DISPLAY SPACE
           DISPLAY "1. Attaque normale"
           DISPLAY "2. Compétence spéciale"
           DISPLAY "3. Utiliser objet"
           DISPLAY "4. Tenter de fuir"
           DISPLAY "Votre choix: " WITH NO ADVANCING
      
           ACCEPT COMBAT-CHOICE
      
           EVALUATE COMBAT-CHOICE
               WHEN 1
                   PERFORM PLAYER-ATTACK
               WHEN 2
                   PERFORM PLAYER-SPECIAL-SKILL
               WHEN 3
                   PERFORM PLAYER-USE-ITEM
               WHEN 4
                   PERFORM PLAYER-ESCAPE
               WHEN OTHER
                   DISPLAY "Action non reconnue!"
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Attaque normale du joueur
      *-----------------------------------------------------------------
       PLAYER-ATTACK.
           COMPUTE RANDOM-FACTOR = FUNCTION RANDOM * 0.3 + 0.85
           COMPUTE ATTACK-POWER = CHAR-STRENGTH * RANDOM-FACTOR
           COMPUTE DAMAGE-DEALT = FUNCTION MAX(
               ATTACK-POWER - ENEMY-DEFENSE, 1)
      
           DISPLAY "Vous attaquez le ", ENEMY-NAME, " et infligez ",
                   DAMAGE-DEALT, " points de dégâts!"
      
           SUBTRACT DAMAGE-DEALT FROM ENEMY-HEALTH-CURRENT
      
           IF ENEMY-HEALTH-CURRENT <= 0
               MOVE 0 TO ENEMY-HEALTH-CURRENT
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Utilisation de compétence spéciale
      *-----------------------------------------------------------------
       PLAYER-SPECIAL-SKILL.
           DISPLAY "Compétences disponibles:"
           DISPLAY "1. ", SKILL-NAME(1), " (Coût: ", SKILL-COST(1), " PM)"
           DISPLAY "2. ", SKILL-NAME(2), " (Coût: ", SKILL-COST(2), " PM)"
           DISPLAY "3. Retour"
           DISPLAY "Votre choix: " WITH NO ADVANCING
      
           ACCEPT COMBAT-CHOICE
      
           EVALUATE COMBAT-CHOICE
               WHEN 1
                   IF CHAR-MANA-CURRENT >= SKILL-COST(1)
                       SUBTRACT SKILL-COST(1) FROM CHAR-MANA-CURRENT
                       PERFORM USE-SKILL-1
                   ELSE
                       DISPLAY "Pas assez de points de mana!"
                   END-IF
               WHEN 2
                   IF CHAR-MANA-CURRENT >= SKILL-COST(2)
                       SUBTRACT SKILL-COST(2) FROM CHAR-MANA-CURRENT
                       PERFORM USE-SKILL-2
                   ELSE
                       DISPLAY "Pas assez de points de mana!"
                   END-IF
               WHEN 3
                   PERFORM PLAYER-COMBAT-TURN
               WHEN OTHER
                   DISPLAY "Action non reconnue!"
                   PERFORM PLAYER-SPECIAL-SKILL
           END-EVALUATE
           .
      
       USE-SKILL-1.
           COMPUTE RANDOM-FACTOR = FUNCTION RANDOM * 0.2 + 0.9
           COMPUTE DAMAGE-DEALT = SKILL-POWER(1) * CHAR-STRENGTH 
                              * RANDOM-FACTOR / 5
      
           DISPLAY "Vous utilisez ", SKILL-NAME(1), "!"
           DISPLAY "Vous infligez ", DAMAGE-DEALT, " points de dégâts!"
      
           SUBTRACT DAMAGE-DEALT FROM ENEMY-HEALTH-CURRENT
      
           IF ENEMY-HEALTH-CURRENT <= 0
               MOVE 0 TO ENEMY-HEALTH-CURRENT
           END-IF
           .
      
       USE-SKILL-2.
           DISPLAY "Vous utilisez ", SKILL-NAME(2), "!"
           DISPLAY "Votre défense augmente pour ce tour!"
           ADD 5 TO CHAR-DEFENSE
           .
      
      *-----------------------------------------------------------------
      * Utilisation d'objet
      *-----------------------------------------------------------------
       PLAYER-USE-ITEM.
           DISPLAY "Objets disponibles:"
           DISPLAY "1. Potion de soin (restaure 15 PV)"
           DISPLAY "2. Élixir de mana (restaure 10 PM)"
           DISPLAY "3. Retour"
           DISPLAY "Votre choix: " WITH NO ADVANCING
      
           ACCEPT COMBAT-CHOICE
      
           EVALUATE COMBAT-CHOICE
               WHEN 1
                   ADD 15 TO CHAR-HEALTH-CURRENT
                   IF CHAR-HEALTH-CURRENT > CHAR-HEALTH-MAX
                       MOVE CHAR-HEALTH-MAX TO CHAR-HEALTH-CURRENT
                   END-IF
                   DISPLAY "Vous récupérez 15 points de vie."
               WHEN 2
                   ADD 10 TO CHAR-MANA-CURRENT
                   IF CHAR-MANA-CURRENT > CHAR-MANA-MAX
                       MOVE CHAR-MANA-MAX TO CHAR-MANA-CURRENT
                   END-IF
                   DISPLAY "Vous récupérez 10 points de mana."
               WHEN 3
                   PERFORM PLAYER-COMBAT-TURN
               WHEN OTHER
                   DISPLAY "Action non reconnue!"
                   PERFORM PLAYER-USE-ITEM
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Tentative de fuite
      *-----------------------------------------------------------------
       PLAYER-ESCAPE.
           COMPUTE ESCAPE-CHANCE = 40 + (CHAR-AGILITY - ENEMY-LEVEL * 2)
           IF ESCAPE-CHANCE < 20
               MOVE 20 TO ESCAPE-CHANCE
           END-IF
      
           COMPUTE RANDOM-FACTOR = FUNCTION RANDOM * 100
      
           IF RANDOM-FACTOR <= ESCAPE-CHANCE
               DISPLAY "Vous parvenez à fuir le combat!"
               MOVE 'E' TO COMBAT-STATUS
           ELSE
               DISPLAY "Vous ne parvenez pas à fuir!"
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Tour de combat de l'ennemi
      *-----------------------------------------------------------------
       ENEMY-COMBAT-TURN.
           COMPUTE RANDOM-FACTOR = FUNCTION RANDOM * 0.3 + 0.85
           COMPUTE ATTACK-POWER = ENEMY-ATTACK * RANDOM-FACTOR
           COMPUTE DAMAGE-DEALT = FUNCTION MAX(
               ATTACK-POWER - CHAR-DEFENSE, 1)
      
           DISPLAY ENEMY-NAME, " vous attaque et inflige ",
                   DAMAGE-DEALT, " points de dégâts!"
      
           SUBTRACT DAMAGE-DEALT FROM CHAR-HEALTH-CURRENT
      
           IF CHAR-HEALTH-CURRENT <= 0
               MOVE 0 TO CHAR-HEALTH-CURRENT
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Vérification de l'état du combat
      *-----------------------------------------------------------------
       CHECK-COMBAT-STATUS.
           IF ENEMY-HEALTH-CURRENT <= 0
               MOVE 'V' TO COMBAT-STATUS
               DISPLAY "Vous avez vaincu le ", ENEMY-NAME, "!"
               DISPLAY "Vous gagnez ", ENEMY-EXPERIENCE, " points d'expérience!"
               MOVE ENEMY-EXPERIENCE TO EXPERIENCE-GAINED
               PERFORM ADD-EXPERIENCE
           END-IF
      
           IF CHAR-HEALTH-CURRENT <= 0
               MOVE 'D' TO COMBAT-STATUS
               DISPLAY "Vous avez été vaincu par le ", ENEMY-NAME, "!"
               DISPLAY "Game Over"
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Changement de tour
      *-----------------------------------------------------------------
       SWITCH-TURNS.
           IF PLAYER-TURN
               MOVE 'E' TO CURRENT-ACTOR
           ELSE
               MOVE 'P' TO CURRENT-ACTOR
           END-IF
           .
      
       END PROGRAM COMBAT-SYSTEM.