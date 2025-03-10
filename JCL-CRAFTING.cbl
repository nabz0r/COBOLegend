      *****************************************************************
      * JCL-CRAFTING.CBL - Système de fabrication d'objets pour COBOLegend
      *
      * Ce module permet aux joueurs de créer des objets en utilisant
      * la syntaxe Job Control Language (JCL), renforçant l'immersion
      * dans l'univers des mainframes.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JCL-CRAFTING-SYSTEM.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Structure du système de crafting
      *-----------------------------------------------------------------
       01 CRAFTING-SYSTEM.
          05 CURRENT-MODE             PIC X(1)   VALUE 'M'.
             88 MODE-MENU             VALUE 'M'.
             88 MODE-EDITOR           VALUE 'E'.
             88 MODE-RESULT           VALUE 'R'.
          05 CRAFT-SUCCESS            PIC X(1)   VALUE 'N'.
             88 CRAFT-SUCCEEDED       VALUE 'Y'.
          05 EDITOR-CONTENT           PIC X(500) VALUE SPACES.
          05 CURRENT-LINE             PIC 9(2)   VALUE 1.
          05 MAX-LINES                PIC 9(2)   VALUE 10.
          05 EDITOR-LINES OCCURS 10 TIMES.
             10 LINE-CONTENT          PIC X(50)  VALUE SPACES.
      
      *-----------------------------------------------------------------
      * Recettes disponibles
      *-----------------------------------------------------------------
       01 RECIPES-TABLE.
          05 RECIPE-COUNT             PIC 9(2)   VALUE 10.
          05 RECIPES OCCURS 10 TIMES.
             10 RECIPE-ID             PIC 9(2).
             10 RECIPE-NAME           PIC X(30).
             10 RECIPE-DESCRIPTION    PIC X(100).
             10 RECIPE-REQUIREMENTS OCCURS 3 TIMES.
                15 REQ-NAME           PIC X(20).
                15 REQ-QUANTITY       PIC 9(2).
             10 RECIPE-JCL-PATTERN    PIC X(100).
             10 RECIPE-RESULT-ITEM    PIC X(30).
             10 RECIPE-RESULT-TYPE    PIC X(1).
                88 RESULT-WEAPON      VALUE 'W'.
                88 RESULT-ARMOR       VALUE 'A'.
                88 RESULT-CONSUMABLE  VALUE 'C'.
                88 RESULT-SPECIAL     VALUE 'S'.
             10 RECIPE-RESULT-POWER   PIC 9(3).
      
      *-----------------------------------------------------------------
      * Matériaux disponibles pour le crafting
      *-----------------------------------------------------------------
       01 MATERIALS-TABLE.
          05 MATERIAL-COUNT          PIC 9(2)   VALUE 8.
          05 MATERIALS OCCURS 15 TIMES.
             10 MATERIAL-ID          PIC 9(2).
             10 MATERIAL-NAME        PIC X(20).
             10 MATERIAL-DESC        PIC X(50).
             10 MATERIAL-QUANTITY    PIC 9(2)   VALUE 0.
      
      *-----------------------------------------------------------------
      * Variables d'interface utilisateur
      *-----------------------------------------------------------------
       01 USER-CHOICE                PIC 9(2)   VALUE 0.
       01 USER-INPUT                 PIC X(50)  VALUE SPACES.
       01 SELECTED-RECIPE            PIC 9(2)   VALUE 0.
       01 HAS-REQUIREMENTS           PIC X(1)   VALUE 'N'.
       01 LINE-NUMBER                PIC 9(2)   VALUE 0.
       01 I                          PIC 9(2)   VALUE 0.
       01 J                          PIC 9(2)   VALUE 0.
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation du système de crafting
      *-----------------------------------------------------------------
       INITIALIZE-CRAFTING.
           PERFORM INIT-MATERIALS
           PERFORM INIT-RECIPES
           .
      
      *-----------------------------------------------------------------
      * Initialisation des matériaux
      *-----------------------------------------------------------------
       INIT-MATERIALS.
           MOVE 1 TO MATERIAL-ID(1)
           MOVE "Fragments binaires" TO MATERIAL-NAME(1)
           MOVE "Petits morceaux de code binaire" TO MATERIAL-DESC(1)
           MOVE 15 TO MATERIAL-QUANTITY(1)
      
           MOVE 2 TO MATERIAL-ID(2)
           MOVE "Cristaux de données" TO MATERIAL-NAME(2)
           MOVE "Cristaux contenant des données structurées" TO MATERIAL-DESC(2)
           MOVE 8 TO MATERIAL-QUANTITY(2)
      
           MOVE 3 TO MATERIAL-ID(3)
           MOVE "Métal recyclé" TO MATERIAL-NAME(3)
           MOVE "Métal récupéré des anciennes machines" TO MATERIAL-DESC(3)
           MOVE 12 TO MATERIAL-QUANTITY(3)
      
           MOVE 4 TO MATERIAL-ID(4)
           MOVE "Essence logique" TO MATERIAL-NAME(4)
           MOVE "Substance qui permet de manipuler la logique" TO MATERIAL-DESC(4)
           MOVE 5 TO MATERIAL-QUANTITY(4)
      
           MOVE 5 TO MATERIAL-ID(5)
           MOVE "Circuit imprimé" TO MATERIAL-NAME(5)
           MOVE "Plaque de circuits basiques" TO MATERIAL-DESC(5)
           MOVE 7 TO MATERIAL-QUANTITY(5)
      
           MOVE 6 TO MATERIAL-ID(6)
           MOVE "Herbes numériques" TO MATERIAL-NAME(6)
           MOVE "Plantes qui poussent dans le cyberespace" TO MATERIAL-DESC(6)
           MOVE 20 TO MATERIAL-QUANTITY(6)
      
           MOVE 7 TO MATERIAL-ID(7)
           MOVE "Fils quantiques" TO MATERIAL-NAME(7)
           MOVE "Fils qui connectent différents états quantiques" TO MATERIAL-DESC(7)
           MOVE 3 TO MATERIAL-QUANTITY(7)
      
           MOVE 8 TO MATERIAL-ID(8)
           MOVE "Mémoire volatile" TO MATERIAL-NAME(8)
           MOVE "Fragments de mémoire RAM instable" TO MATERIAL-DESC(8)
           MOVE 9 TO MATERIAL-QUANTITY(8)
           .
      
      *-----------------------------------------------------------------
      * Initialisation des recettes
      *-----------------------------------------------------------------
       INIT-RECIPES.
           *> Recette 1: Épée de Compilation
           MOVE 1 TO RECIPE-ID(1)
           MOVE "Épée de Compilation" TO RECIPE-NAME(1)
           MOVE "Une épée qui compile les bugs en code fonctionnel" 
                TO RECIPE-DESCRIPTION(1)
           
           MOVE "Métal recyclé" TO REQ-NAME(1, 1)
           MOVE 3 TO REQ-QUANTITY(1, 1)
           MOVE "Cristaux de données" TO REQ-NAME(1, 2)
           MOVE 2 TO REQ-QUANTITY(1, 2)
           MOVE "Essence logique" TO REQ-NAME(1, 3)
           MOVE 1 TO REQ-QUANTITY(1, 3)
      
           MOVE "//COMPILE JOB*" TO RECIPE-JCL-PATTERN(1)
           MOVE "Épée de Compilation" TO RECIPE-RESULT-ITEM(1)
           MOVE "W" TO RECIPE-RESULT-TYPE(1)
           MOVE 15 TO RECIPE-RESULT-POWER(1)
      
           *> Recette 2: Armure de Cache
           MOVE 2 TO RECIPE-ID(2)
           MOVE "Armure de Cache" TO RECIPE-NAME(2)
           MOVE "Une armure qui protège des erreurs d'exécution" 
                TO RECIPE-DESCRIPTION(2)
           
           MOVE "Métal recyclé" TO REQ-NAME(2, 1)
           MOVE 4 TO REQ-QUANTITY(2, 1)
           MOVE "Circuit imprimé" TO REQ-NAME(2, 2)
           MOVE 2 TO REQ-QUANTITY(2, 2)
           MOVE "Fils quantiques" TO REQ-NAME(2, 3)
           MOVE 1 TO REQ-QUANTITY(2, 3)
      
           MOVE "//PROTECT JOB*" TO RECIPE-JCL-PATTERN(2)
           MOVE "Armure de Cache" TO RECIPE-RESULT-ITEM(2)
           MOVE "A" TO RECIPE-RESULT-TYPE(2)
           MOVE 12 TO RECIPE-RESULT-POWER(2)
      
           *> Recette 3: Potion de Débogage
           MOVE 3 TO RECIPE-ID(3)
           MOVE "Potion de Débogage" TO RECIPE-NAME(3)
           MOVE "Une potion qui restaure la santé en éliminant les bugs" 
                TO RECIPE-DESCRIPTION(3)
           
           MOVE "Herbes numériques" TO REQ-NAME(3, 1)
           MOVE 5 TO REQ-QUANTITY(3, 1)
           MOVE "Essence logique" TO REQ-NAME(3, 2)
           MOVE 1 TO REQ-QUANTITY(3, 2)
           MOVE "Mémoire volatile" TO REQ-NAME(3, 3)
           MOVE 2 TO REQ-QUANTITY(3, 3)
      
           MOVE "//DEBUG JOB*" TO RECIPE-JCL-PATTERN(3)
           MOVE "Potion de Débogage" TO RECIPE-RESULT-ITEM(3)
           MOVE "C" TO RECIPE-RESULT-TYPE(3)
           MOVE 30 TO RECIPE-RESULT-POWER(3)
      
           *> Recette 4: Amulette de Traduction
           MOVE 4 TO RECIPE-ID(4)
           MOVE "Amulette de Traduction" TO RECIPE-NAME(4)
           MOVE "Un artefact qui traduit tout langage de programmation" 
                TO RECIPE-DESCRIPTION(4)
           
           MOVE "Cristaux de données" TO REQ-NAME(4, 1)
           MOVE 3 TO REQ-QUANTITY(4, 1)
           MOVE "Fils quantiques" TO REQ-NAME(4, 2)
           MOVE 2 TO REQ-QUANTITY(4, 2)
           MOVE "Fragments binaires" TO REQ-NAME(4, 3)
           MOVE 10 TO REQ-QUANTITY(4, 3)
      
           MOVE "//TRANSLATE JOB*" TO RECIPE-JCL-PATTERN(4)
           MOVE "Amulette de Traduction" TO RECIPE-RESULT-ITEM(4)
           MOVE "S" TO RECIPE-RESULT-TYPE(4)
           MOVE 5 TO RECIPE-RESULT-POWER(4)
      
           *> Recette 5: Marteau d'Assemblage
           MOVE 5 TO RECIPE-ID(5)
           MOVE "Marteau d'Assemblage" TO RECIPE-NAME(5)
           MOVE "Un marteau qui assemble le code en instructions puissantes" 
                TO RECIPE-DESCRIPTION(5)
           
           MOVE "Métal recyclé" TO REQ-NAME(5, 1)
           MOVE 5 TO REQ-QUANTITY(5, 1)
           MOVE "Essence logique" TO REQ-NAME(5, 2)
           MOVE 2 TO REQ-QUANTITY(5, 2)
           MOVE "Circuit imprimé" TO REQ-NAME(5, 3)
           MOVE 1 TO REQ-QUANTITY(5, 3)
      
           MOVE "//ASSEMBLE JOB*" TO RECIPE-JCL-PATTERN(5)
           MOVE "Marteau d'Assemblage" TO RECIPE-RESULT-ITEM(5)
           MOVE "W" TO RECIPE-RESULT-TYPE(5)
           MOVE 18 TO RECIPE-RESULT-POWER(5)
           .
      
      *-----------------------------------------------------------------
      * Menu principal du système de crafting
      *-----------------------------------------------------------------
       CRAFTING-MAIN-MENU.
           MOVE 'M' TO CURRENT-MODE
      
           DISPLAY SPACE
           DISPLAY "***********************************************"
           DISPLAY "*           TERMINAL DE FABRICATION           *"
           DISPLAY "*    Créer des objets avec du code JCL       *"
           DISPLAY "***********************************************"
           DISPLAY SPACE
           DISPLAY "1. Voir les recettes disponibles"
           DISPLAY "2. Vérifier mon inventaire de matériaux"
           DISPLAY "3. Commencer à crafter un objet"
           DISPLAY "4. Quitter le terminal de fabrication"
           DISPLAY SPACE
           DISPLAY "Votre choix: " WITH NO ADVANCING
      
           ACCEPT USER-CHOICE
      
           EVALUATE USER-CHOICE
               WHEN 1
                   PERFORM DISPLAY-RECIPES
               WHEN 2
                   PERFORM DISPLAY-MATERIALS
               WHEN 3
                   PERFORM SELECT-RECIPE-TO-CRAFT
               WHEN 4
                   MOVE 'X' TO CURRENT-MODE
               WHEN OTHER
                   DISPLAY "Choix invalide, veuillez réessayer."
                   PERFORM CRAFTING-MAIN-MENU
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Affichage des recettes disponibles
      *-----------------------------------------------------------------
       DISPLAY-RECIPES.
           DISPLAY SPACE
           DISPLAY "=== RECETTES DISPONIBLES ==="
           DISPLAY SPACE
      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RECIPE-COUNT
               DISPLAY "Recette " I ": " RECIPE-NAME(I)
               DISPLAY "  Description: " RECIPE-DESCRIPTION(I)
               DISPLAY "  Matériaux requis:"
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   IF REQ-NAME(I, J) NOT = SPACES
                       DISPLAY "    - " REQ-NAME(I, J) " x" REQ-QUANTITY(I, J)
                   END-IF
               END-PERFORM
               DISPLAY "  Pattern JCL: " RECIPE-JCL-PATTERN(I)
               DISPLAY "  Résultat: " RECIPE-RESULT-ITEM(I)
               EVALUATE RECIPE-RESULT-TYPE(I)
                   WHEN "W"
                       DISPLAY "    Type: Arme (ATT+" RECIPE-RESULT-POWER(I) ")"
                   WHEN "A"
                       DISPLAY "    Type: Armure (DEF+" RECIPE-RESULT-POWER(I) ")"
                   WHEN "C"
                       DISPLAY "    Type: Consommable (Effet: " 
                              RECIPE-RESULT-POWER(I) ")"
                   WHEN "S"
                       DISPLAY "    Type: Spécial"
               END-EVALUATE
               DISPLAY SPACE
           END-PERFORM
      
           DISPLAY "Appuyez sur ENTRÉE pour revenir au menu..."
           ACCEPT USER-INPUT
           PERFORM CRAFTING-MAIN-MENU
           .
      
      *-----------------------------------------------------------------
      * Affichage des matériaux disponibles
      *-----------------------------------------------------------------
       DISPLAY-MATERIALS.
           DISPLAY SPACE
           DISPLAY "=== MATÉRIAUX DISPONIBLES ==="
           DISPLAY SPACE
      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MATERIAL-COUNT
               DISPLAY I ". " MATERIAL-NAME(I) " (" 
                       MATERIAL-QUANTITY(I) " unités)"
               DISPLAY "   " MATERIAL-DESC(I)
           END-PERFORM
      
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTRÉE pour revenir au menu..."
           ACCEPT USER-INPUT
           PERFORM CRAFTING-MAIN-MENU
           .
      
      *-----------------------------------------------------------------
      * Sélection d'une recette à crafter
      *-----------------------------------------------------------------
       SELECT-RECIPE-TO-CRAFT.
           DISPLAY SPACE
           DISPLAY "=== CHOIX DE LA RECETTE À CRAFTER ==="
           DISPLAY SPACE
      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RECIPE-COUNT
               DISPLAY I ". " RECIPE-NAME(I)
           END-PERFORM
      
           DISPLAY SPACE
           DISPLAY "Choisissez une recette (0 pour annuler): " 
                   WITH NO ADVANCING
           ACCEPT SELECTED-RECIPE
      
           IF SELECTED-RECIPE = 0
               PERFORM CRAFTING-MAIN-MENU
           ELSE
               IF SELECTED-RECIPE > 0 AND SELECTED-RECIPE <= RECIPE-COUNT
                   PERFORM CHECK-RECIPE-REQUIREMENTS
                   IF HAS-REQUIREMENTS = "Y"
                       PERFORM ENTER-JCL-EDITOR
                   ELSE
                       DISPLAY SPACE
                       DISPLAY "Vous n'avez pas assez de matériaux pour cette recette."
                       DISPLAY "Appuyez sur ENTRÉE pour revenir au menu..."
                       ACCEPT USER-INPUT
                       PERFORM CRAFTING-MAIN-MENU
                   END-IF
               ELSE
                   DISPLAY "Choix invalide."
                   PERFORM SELECT-RECIPE-TO-CRAFT
               END-IF
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Vérification des requis pour une recette
      *-----------------------------------------------------------------
       CHECK-RECIPE-REQUIREMENTS.
           MOVE "Y" TO HAS-REQUIREMENTS
      
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               IF REQ-NAME(SELECTED-RECIPE, J) NOT = SPACES
                   PERFORM CHECK-MATERIAL-AVAILABILITY
                   IF MATERIAL-FOUND = "N" OR 
                      MATERIAL-QUANTITY(MATERIAL-INDEX) < 
                      REQ-QUANTITY(SELECTED-RECIPE, J)
                       MOVE "N" TO HAS-REQUIREMENTS
                   END-IF
               END-IF
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Vérification de la disponibilité d'un matériau
      *-----------------------------------------------------------------
       CHECK-MATERIAL-AVAILABILITY.
           MOVE "N" TO MATERIAL-FOUND
           MOVE 0 TO MATERIAL-INDEX
      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MATERIAL-COUNT
               IF MATERIAL-NAME(I) = REQ-NAME(SELECTED-RECIPE, J)
                   MOVE "Y" TO MATERIAL-FOUND
                   MOVE I TO MATERIAL-INDEX
               END-IF
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Interface d'édition JCL
      *-----------------------------------------------------------------
       ENTER-JCL-EDITOR.
           DISPLAY SPACE
           DISPLAY "=== ÉDITEUR JCL ==="
           DISPLAY "Créez votre script JCL en suivant le pattern requis"
           DISPLAY "Pattern pour cette recette: " 
                   RECIPE-JCL-PATTERN(SELECTED-RECIPE)
           DISPLAY SPACE
      
           MOVE 'E' TO CURRENT-MODE
           MOVE SPACES TO EDITOR-CONTENT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-LINES
               MOVE SPACES TO LINE-CONTENT(I)
           END-PERFORM
      
           MOVE 1 TO CURRENT-LINE
           PERFORM EDIT-JCL-CODE
           .
      
      *-----------------------------------------------------------------
      * Édition du code JCL
      *-----------------------------------------------------------------
       EDIT-JCL-CODE.
           PERFORM DISPLAY-EDITOR-CONTENT
      
           DISPLAY "Ligne " CURRENT-LINE ": " WITH NO ADVANCING
           ACCEPT LINE-CONTENT(CURRENT-LINE)
      
           IF LINE-CONTENT(CURRENT-LINE) = "SAVE"
               PERFORM VALIDATE-JCL-CODE
           ELSE IF LINE-CONTENT(CURRENT-LINE) = "EXIT"
               PERFORM CRAFTING-MAIN-MENU
           ELSE
               IF CURRENT-LINE < MAX-LINES
                   ADD 1 TO CURRENT-LINE
               END-IF
               PERFORM EDIT-JCL-CODE
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Affichage du contenu de l'éditeur
      *-----------------------------------------------------------------
       DISPLAY-EDITOR-CONTENT.
           DISPLAY SPACE
           DISPLAY "--- CODE JCL ACTUEL ---"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-LINES
               IF I < CURRENT-LINE
                   DISPLAY I ". " LINE-CONTENT(I)
               ELSE IF I = CURRENT-LINE
                   DISPLAY I ". " LINE-CONTENT(I) " <-- Ligne actuelle"
               ELSE
                   DISPLAY I ". "
               END-IF
           END-PERFORM
           DISPLAY "----------------------"
           DISPLAY "Tapez 'SAVE' pour valider ou 'EXIT' pour annuler"
           DISPLAY SPACE
           .
      
      *-----------------------------------------------------------------
      * Validation du code JCL
      *-----------------------------------------------------------------
       VALIDATE-JCL-CODE.
           MOVE "N" TO CRAFT-SUCCESS
      
           *> Vérifier si le code contient le pattern requis
           MOVE SPACES TO EDITOR-CONTENT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-LINES
               STRING EDITOR-CONTENT DELIMITED BY SIZE
                      LINE-CONTENT(I) DELIMITED BY SIZE
                      INTO EDITOR-CONTENT
           END-PERFORM
      
           *> Contrôler si le pattern requis est présent
           IF EDITOR-CONTENT CONTAINS RECIPE-JCL-PATTERN(SELECTED-RECIPE)
               MOVE "Y" TO CRAFT-SUCCESS
           END-IF
      
           *> Vérifier les erreurs communes de syntaxe JCL
           IF EDITOR-CONTENT NOT CONTAINS "//"
               MOVE "N" TO CRAFT-SUCCESS
           END-IF
      
           IF EDITOR-CONTENT NOT CONTAINS "JOB"
               MOVE "N" TO CRAFT-SUCCESS
           END-IF
      
           *> Afficher le résultat
           MOVE 'R' TO CURRENT-MODE
           PERFORM DISPLAY-CRAFTING-RESULT
           .
      
      *-----------------------------------------------------------------
      * Affichage du résultat du crafting
      *-----------------------------------------------------------------
       DISPLAY-CRAFTING-RESULT.
           DISPLAY SPACE
      
           IF CRAFT-SUCCESS = "Y"
               DISPLAY "=== FABRICATION RÉUSSIE! ==="
               DISPLAY "Votre code JCL a été compilé avec succès!"
               DISPLAY "Vous avez créé: " RECIPE-RESULT-ITEM(SELECTED-RECIPE)
      
               *> Soustraire les matériaux utilisés
               PERFORM CONSUME-MATERIALS
      
               *> Décrire l'objet créé
               DISPLAY SPACE
               DISPLAY "Description de l'objet:"
               DISPLAY RECIPE-DESCRIPTION(SELECTED-RECIPE)
      
               EVALUATE RECIPE-RESULT-TYPE(SELECTED-RECIPE)
                   WHEN "W"
                       DISPLAY "Type: Arme"
                       DISPLAY "Puissance d'attaque: +" 
                              RECIPE-RESULT-POWER(SELECTED-RECIPE)
                   WHEN "A"
                       DISPLAY "Type: Armure"
                       DISPLAY "Puissance de défense: +" 
                              RECIPE-RESULT-POWER(SELECTED-RECIPE)
                   WHEN "C"
                       DISPLAY "Type: Consommable"
                       DISPLAY "Effet de guérison: +" 
                              RECIPE-RESULT-POWER(SELECTED-RECIPE) " PV"
                   WHEN "S"
                       DISPLAY "Type: Objet spécial"
                       DISPLAY "Effet unique: Voir description"
               END-EVALUATE
           ELSE
               DISPLAY "=== ÉCHEC DE FABRICATION ==="
               DISPLAY "Votre code JCL contient des erreurs ou ne correspond"
               DISPLAY "pas au pattern requis pour cette recette."
               DISPLAY SPACE
               DISPLAY "Conseil: Assurez-vous que votre code contient le pattern"
               DISPLAY "'" RECIPE-JCL-PATTERN(SELECTED-RECIPE) "' et respecte"
               DISPLAY "la syntaxe JCL de base (commencez par // et incluez JOB)."
           END-IF
      
           DISPLAY SPACE
           DISPLAY "1. Retourner au menu principal"
           DISPLAY "2. Essayer à nouveau avec la même recette"
           DISPLAY "Votre choix: " WITH NO ADVANCING
           ACCEPT USER-CHOICE
      
           IF USER-CHOICE = 2
               PERFORM ENTER-JCL-EDITOR
           ELSE
               PERFORM CRAFTING-MAIN-MENU
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Consommation des matériaux pour le crafting
      *-----------------------------------------------------------------
       CONSUME-MATERIALS.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
               IF REQ-NAME(SELECTED-RECIPE, J) NOT = SPACES
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > MATERIAL-COUNT
                       IF MATERIAL-NAME(I) = REQ-NAME(SELECTED-RECIPE, J)
                           SUBTRACT REQ-QUANTITY(SELECTED-RECIPE, J) 
                               FROM MATERIAL-QUANTITY(I)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
      
       END PROGRAM JCL-CRAFTING-SYSTEM.
