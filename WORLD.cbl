      *****************************************************************
      * WORLD.CBL - Système de gestion du monde de COBOLegend
      *
      * Ce module gère la carte du monde, les zones, les déplacements
      * et les interactions avec l'environnement.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORLD-SYSTEM.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Structure de la carte du monde
      *-----------------------------------------------------------------
       01 WORLD-MAP.
          05 MAP-SIZE-X              PIC 9(2)   VALUE 10.
          05 MAP-SIZE-Y              PIC 9(2)   VALUE 10.
          05 MAP-LOCATIONS OCCURS 10 TIMES INDEXED BY LOC-X.
             10 MAP-LOCATION OCCURS 10 TIMES INDEXED BY LOC-Y.
                15 LOCATION-ID        PIC 9(3).
                15 LOCATION-NAME      PIC X(25).
                15 LOCATION-DESC      PIC X(255).
                15 LOCATION-TYPE      PIC X(1).
                   88 LOC-TOWN        VALUE 'T'.
                   88 LOC-DUNGEON     VALUE 'D'.
                   88 LOC-WILDERNESS  VALUE 'W'.
                   88 LOC-SPECIAL     VALUE 'S'.
                15 LOCATION-ENCOUNTER-RATE PIC 9(2).
                15 LOCATION-VISITED   PIC X(1).
                   88 LOC-IS-VISITED  VALUE 'Y'.
                15 LOCATION-EXITS.
                   20 EXIT-NORTH      PIC X(1).
                      88 HAS-NORTH-EXIT VALUE 'Y'.
                   20 EXIT-SOUTH      PIC X(1).
                      88 HAS-SOUTH-EXIT VALUE 'Y'.
                   20 EXIT-EAST       PIC X(1).
                      88 HAS-EAST-EXIT  VALUE 'Y'.
                   20 EXIT-WEST       PIC X(1).
                      88 HAS-WEST-EXIT  VALUE 'Y'.
      
      *-----------------------------------------------------------------
      * Position du joueur sur la carte
      *-----------------------------------------------------------------
       01 PLAYER-POSITION.
          05 PLAYER-POS-X            PIC 9(2)   VALUE 5.
          05 PLAYER-POS-Y            PIC 9(2)   VALUE 5.
      
      *-----------------------------------------------------------------
      * Gestion des rencontres aléatoires
      *-----------------------------------------------------------------
       01 ENCOUNTER-CHECK.
          05 ENCOUNTER-CHANCE        PIC 9(3).
          05 RANDOM-NUMBER           PIC 9(3).
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation de la carte du monde
      *-----------------------------------------------------------------
       INITIALIZE-WORLD.
           PERFORM VARYING LOC-X FROM 1 BY 1 UNTIL LOC-X > MAP-SIZE-X
               PERFORM VARYING LOC-Y FROM 1 BY 1 
                       UNTIL LOC-Y > MAP-SIZE-Y
                   COMPUTE LOCATION-ID(LOC-X, LOC-Y) = 
                      ((LOC-X - 1) * MAP-SIZE-Y) + LOC-Y
                   MOVE "Zone inexplorée" 
                       TO LOCATION-NAME(LOC-X, LOC-Y)
                   MOVE "Une zone que vous n'avez pas encore explorée." 
                       TO LOCATION-DESC(LOC-X, LOC-Y)
                   MOVE "W" TO LOCATION-TYPE(LOC-X, LOC-Y)
                   MOVE 10 TO LOCATION-ENCOUNTER-RATE(LOC-X, LOC-Y)
                   MOVE "N" TO LOCATION-VISITED(LOC-X, LOC-Y)
                   MOVE "Y" TO EXIT-NORTH(LOC-X, LOC-Y)
                   MOVE "Y" TO EXIT-SOUTH(LOC-X, LOC-Y)
                   MOVE "Y" TO EXIT-EAST(LOC-X, LOC-Y)
                   MOVE "Y" TO EXIT-WEST(LOC-X, LOC-Y)
               END-PERFORM
           END-PERFORM
      
           PERFORM CONFIGURE-SPECIFIC-LOCATIONS
           .
      
      *-----------------------------------------------------------------
      * Configuration des zones spécifiques
      *-----------------------------------------------------------------
       CONFIGURE-SPECIFIC-LOCATIONS.
           MOVE "Village de Départ" TO LOCATION-NAME(5, 5)
           MOVE "Un petit village paisible. C'est votre point de départ "
                "pour l'aventure." TO LOCATION-DESC(5, 5)
           MOVE "T" TO LOCATION-TYPE(5, 5)
           MOVE 0 TO LOCATION-ENCOUNTER-RATE(5, 5)
           MOVE "Y" TO LOCATION-VISITED(5, 5)
      
           MOVE "Forêt Dense" TO LOCATION-NAME(6, 5)
           MOVE "Une forêt dense et mystérieuse. Des bruits étranges "
                "proviennent de l'intérieur." TO LOCATION-DESC(6, 5)
           MOVE "W" TO LOCATION-TYPE(6, 5)
           MOVE 30 TO LOCATION-ENCOUNTER-RATE(6, 5)
      
           MOVE "Entrée de la Cité" TO LOCATION-NAME(8, 5)
           MOVE "L'entrée imposante de la cité futuriste. Des gardes "
                "contrôlent les allées et venues." TO LOCATION-DESC(8, 5)
           MOVE "S" TO LOCATION-TYPE(8, 5)
           MOVE 0 TO LOCATION-ENCOUNTER-RATE(8, 5)
           .
      
      *-----------------------------------------------------------------
      * Déplacement du joueur sur la carte
      *-----------------------------------------------------------------
       MOVE-PLAYER-NORTH.
           IF PLAYER-POS-Y > 1 AND 
              HAS-NORTH-EXIT(PLAYER-POS-X, PLAYER-POS-Y)
               SUBTRACT 1 FROM PLAYER-POS-Y
               DISPLAY "Vous vous déplacez vers le nord."
               PERFORM PROCESS-NEW-LOCATION
           ELSE
               DISPLAY "Vous ne pouvez pas aller dans cette direction."
           END-IF
           .
      
       MOVE-PLAYER-SOUTH.
           IF PLAYER-POS-Y < MAP-SIZE-Y AND 
              HAS-SOUTH-EXIT(PLAYER-POS-X, PLAYER-POS-Y)
               ADD 1 TO PLAYER-POS-Y
               DISPLAY "Vous vous déplacez vers le sud."
               PERFORM PROCESS-NEW-LOCATION
           ELSE
               DISPLAY "Vous ne pouvez pas aller dans cette direction."
           END-IF
           .
      
       MOVE-PLAYER-EAST.
           IF PLAYER-POS-X < MAP-SIZE-X AND 
              HAS-EAST-EXIT(PLAYER-POS-X, PLAYER-POS-Y)
               ADD 1 TO PLAYER-POS-X
               DISPLAY "Vous vous déplacez vers l'est."
               PERFORM PROCESS-NEW-LOCATION
           ELSE
               DISPLAY "Vous ne pouvez pas aller dans cette direction."
           END-IF
           .
      
       MOVE-PLAYER-WEST.
           IF PLAYER-POS-X > 1 AND 
              HAS-WEST-EXIT(PLAYER-POS-X, PLAYER-POS-Y)
               SUBTRACT 1 FROM PLAYER-POS-X
               DISPLAY "Vous vous déplacez vers l'ouest."
               PERFORM PROCESS-NEW-LOCATION
           ELSE
               DISPLAY "Vous ne pouvez pas aller dans cette direction."
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Traitement d'une nouvelle localisation
      *-----------------------------------------------------------------
       PROCESS-NEW-LOCATION.
           MOVE "Y" TO LOCATION-VISITED(PLAYER-POS-X, PLAYER-POS-Y)
      
           DISPLAY SPACE
           DISPLAY LOCATION-NAME(PLAYER-POS-X, PLAYER-POS-Y)
           DISPLAY LOCATION-DESC(PLAYER-POS-X, PLAYER-POS-Y)
      
           DISPLAY "Sorties disponibles: " WITH NO ADVANCING
           IF HAS-NORTH-EXIT(PLAYER-POS-X, PLAYER-POS-Y)
               DISPLAY "Nord " WITH NO ADVANCING
           END-IF
           IF HAS-SOUTH-EXIT(PLAYER-POS-X, PLAYER-POS-Y)
               DISPLAY "Sud " WITH NO ADVANCING
           END-IF
           IF HAS-EAST-EXIT(PLAYER-POS-X, PLAYER-POS-Y)
               DISPLAY "Est " WITH NO ADVANCING
           END-IF
           IF HAS-WEST-EXIT(PLAYER-POS-X, PLAYER-POS-Y)
               DISPLAY "Ouest" WITH NO ADVANCING
           END-IF
           DISPLAY SPACE
      
           PERFORM CHECK-FOR-ENCOUNTER
           .
      
      *-----------------------------------------------------------------
      * Vérification des rencontres aléatoires
      *-----------------------------------------------------------------
       CHECK-FOR-ENCOUNTER.
           IF LOCATION-TYPE(PLAYER-POS-X, PLAYER-POS-Y) = "W" OR 
              LOCATION-TYPE(PLAYER-POS-X, PLAYER-POS-Y) = "D"
      
               MOVE LOCATION-ENCOUNTER-RATE(PLAYER-POS-X, PLAYER-POS-Y) 
                   TO ENCOUNTER-CHANCE
      
               COMPUTE RANDOM-NUMBER = FUNCTION RANDOM * 100
      
               IF RANDOM-NUMBER <= ENCOUNTER-CHANCE
                   PERFORM TRIGGER-RANDOM-ENCOUNTER
               END-IF
           END-IF
           .
      
       TRIGGER-RANDOM-ENCOUNTER.
           DISPLAY "Une rencontre aléatoire se produit!"
           EVALUATE LOCATION-TYPE(PLAYER-POS-X, PLAYER-POS-Y)
               WHEN "W"
                   PERFORM WILDERNESS-ENCOUNTER
               WHEN "D"
                   PERFORM DUNGEON-ENCOUNTER
           END-EVALUATE
           .
      
       WILDERNESS-ENCOUNTER.
           DISPLAY "Vous rencontrez un ennemi dans la nature!"
           MOVE "Loup binaire" TO ENEMY-NAME
           MOVE 2 TO ENEMY-LEVEL
           MOVE 15 TO ENEMY-HEALTH-CURRENT
           MOVE 15 TO ENEMY-HEALTH-MAX
           MOVE 6 TO ENEMY-ATTACK
           MOVE 2 TO ENEMY-DEFENSE
           MOVE 25 TO ENEMY-EXPERIENCE
      
           PERFORM INITIALIZE-COMBAT
           PERFORM COMBAT-LOOP
           .
      
       DUNGEON-ENCOUNTER.
           DISPLAY "Vous rencontrez un ennemi dans le donjon!"
           MOVE "Golem de données" TO ENEMY-NAME
           MOVE 4 TO ENEMY-LEVEL
           MOVE 30 TO ENEMY-HEALTH-CURRENT
           MOVE 30 TO ENEMY-HEALTH-MAX
           MOVE 8 TO ENEMY-ATTACK
           MOVE 5 TO ENEMY-DEFENSE
           MOVE 50 TO ENEMY-EXPERIENCE
      
           PERFORM INITIALIZE-COMBAT
           PERFORM COMBAT-LOOP
           .
      
      *-----------------------------------------------------------------
      * Affichage de la carte (version simplifiée)
      *-----------------------------------------------------------------
       DISPLAY-WORLD-MAP.
           DISPLAY SPACE
           DISPLAY "=== CARTE DU MONDE ==="
           DISPLAY "Légende: [P] Position actuelle, [T] Ville, "
                   "[D] Donjon, [W] Nature, [?] Inexploré"
           DISPLAY SPACE
      
           PERFORM VARYING LOC-Y FROM 1 BY 1 UNTIL LOC-Y > MAP-SIZE-Y
               PERFORM VARYING LOC-X FROM 1 BY 1 
                       UNTIL LOC-X > MAP-SIZE-X
                   IF PLAYER-POS-X = LOC-X AND PLAYER-POS-Y = LOC-Y
                       DISPLAY "[P]" WITH NO ADVANCING
                   ELSE
                       IF LOC-IS-VISITED(LOC-X, LOC-Y)
                           DISPLAY "[", LOCATION-TYPE(LOC-X, LOC-Y), "]"
                               WITH NO ADVANCING
                       ELSE
                           DISPLAY "[?]" WITH NO ADVANCING
                       END-IF
                   END-IF
      
                   IF LOC-X = MAP-SIZE-X
                       DISPLAY SPACE
                   END-IF
               END-PERFORM
           END-PERFORM
           .
      
       END PROGRAM WORLD-SYSTEM.
