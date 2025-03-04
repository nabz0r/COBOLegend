      *****************************************************************
      * DIALOG-MULTIPLEXER.CBL - Système de dialogue pour COBOLegend
      *
      * Ce module gère toutes les interactions conversationnelles avec
      * les personnages non-joueurs (PNJ), permettant des dialogues
      * ramifiés et des choix conditionnels.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIALOG-MULTIPLEXER.
       AUTHOR. CLAUDE.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Structure des dialogues
      *-----------------------------------------------------------------
       01 DIALOG-SYSTEM.
          05 CURRENT-DIALOG-ID       PIC 9(3)   VALUE 0.
          05 CURRENT-NODE-ID         PIC 9(3)   VALUE 0.
          05 DIALOG-ACTIVE           PIC X(1)   VALUE "N".
             88 IS-DIALOG-ACTIVE     VALUE "Y".
          05 SPEAKER-NAME            PIC X(30)  VALUE SPACES.
          05 MAX-DIALOG-OPTIONS      PIC 9(1)   VALUE 5.
          05 DIALOG-CHOICE           PIC 9(1)   VALUE 0.
      
      *-----------------------------------------------------------------
      * Base de données de dialogues
      *-----------------------------------------------------------------
       01 DIALOG-DATABASE.
          05 DIALOG-COUNT            PIC 9(3)   VALUE 4.
          05 DIALOG-ENTRIES OCCURS 10 TIMES.
             10 DIALOG-ID            PIC 9(3).
             10 DIALOG-NPC-NAME      PIC X(30).
             10 DIALOG-LOCATION      PIC X(30).
             10 DIALOG-CONDITION     PIC X(50).
             10 DIALOG-ROOT-NODE     PIC 9(3).
      
      *-----------------------------------------------------------------
      * Nœuds de dialogue
      *-----------------------------------------------------------------
       01 DIALOG-NODES.
          05 NODE-COUNT              PIC 9(3)   VALUE 15.
          05 DIALOG-NODE OCCURS 30 TIMES.
             10 NODE-ID              PIC 9(3).
             10 NODE-DIALOG-ID       PIC 9(3).
             10 NODE-TEXT            PIC X(255).
             10 NODE-SPEAKER         PIC X(30).
             10 NODE-OPTIONS-COUNT   PIC 9(1)   VALUE 0.
             10 NODE-OPTIONS OCCURS 5 TIMES.
                15 OPTION-TEXT       PIC X(80).
                15 OPTION-NEXT-NODE  PIC 9(3).
                15 OPTION-CONDITION  PIC X(50).
                15 OPTION-ACTION     PIC X(50).
      
      *-----------------------------------------------------------------
      * Variables de statut des PNJ
      *-----------------------------------------------------------------
       01 NPC-STATUSES.
          05 NPC-TALKED-TO           PIC X(20)  VALUE SPACES.
          05 NPC-RELATIONSHIP        PIC 9(3)   VALUE 50.
          05 NPC-QUEST-OFFERED       PIC X(1)   VALUE "N".
          05 NPC-QUEST-COMPLETED     PIC X(1)   VALUE "N".
      
      *-----------------------------------------------------------------
      * Variables temporaires
      *-----------------------------------------------------------------
       01 DISPLAY-INDEX              PIC 9(1)   VALUE 0.
       01 VALID-OPTIONS-COUNT        PIC 9(1)   VALUE 0.
       01 OPTION-IS-VALID            PIC X(1)   VALUE "Y".
       01 TEXT-PART                  PIC X(80)  VALUE SPACES.
       01 TEXT-LENGTH                PIC 9(3)   VALUE 0.
       01 TEXT-POS                   PIC 9(3)   VALUE 0.
       01 DISPLAY-WIDTH              PIC 9(2)   VALUE 60.
       01 CHAR-COUNTER               PIC 9(3)   VALUE 0.
       01 CONDITION-MET              PIC X(1)   VALUE "N".
       01 NODE-FOUND                 PIC X(1)   VALUE "N".
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation des dialogues
      *-----------------------------------------------------------------
       INITIALIZE-DIALOGS.
           PERFORM SETUP-DIALOG-DATABASE
           PERFORM SETUP-DIALOG-NODES
           .
      
      *-----------------------------------------------------------------
      * Configuration de la base de données de dialogues
      *-----------------------------------------------------------------
       SETUP-DIALOG-DATABASE.
           MOVE 1 TO DIALOG-ID(1)
           MOVE "Archiviste Ada" TO DIALOG-NPC-NAME(1)
           MOVE "Bibliothèque Centrale" TO DIALOG-LOCATION(1)
           MOVE "NONE" TO DIALOG-CONDITION(1)
           MOVE 1 TO DIALOG-ROOT-NODE(1)
      
           MOVE 2 TO DIALOG-ID(2)
           MOVE "Technicien Turing" TO DIALOG-NPC-NAME(2)
           MOVE "Salle des Machines" TO DIALOG-LOCATION(2)
           MOVE "NONE" TO DIALOG-CONDITION(2)
           MOVE 6 TO DIALOG-ROOT-NODE(2)
      
           MOVE 3 TO DIALOG-ID(3)
           MOVE "Gardien Neumann" TO DIALOG-NPC-NAME(3)
           MOVE "Entrée de la Cité" TO DIALOG-LOCATION(3)
           MOVE "MAIN_QUEST_1_ACTIVE" TO DIALOG-CONDITION(3)
           MOVE 10 TO DIALOG-ROOT-NODE(3)
           
           MOVE 4 TO DIALOG-ID(4)
           MOVE "Voyageur Temporel" TO DIALOG-NPC-NAME(4)
           MOVE "Terminal Central" TO DIALOG-LOCATION(4)
           MOVE "TIME_TRAVEL_UNLOCKED" TO DIALOG-CONDITION(4)
           MOVE 13 TO DIALOG-ROOT-NODE(4)
           .
      
      *-----------------------------------------------------------------
      * Configuration des nœuds de dialogue
      *-----------------------------------------------------------------
       SETUP-DIALOG-NODES.
           *> Dialogue 1: Archiviste Ada
           MOVE 1 TO NODE-ID(1)
           MOVE 1 TO NODE-DIALOG-ID(1)
           MOVE "Bonjour, voyageur. Bienvenue dans la Bibliothèque "
                "Centrale de MAINFRAME-TERRA. Je suis Ada, gardienne "
                "des connaissances anciennes." 
                TO NODE-TEXT(1)
           MOVE "Archiviste Ada" TO NODE-SPEAKER(1)
           MOVE 3 TO NODE-OPTIONS-COUNT(1)
           MOVE "Parlez-moi de cette bibliothèque." 
                TO OPTION-TEXT(1, 1)
           MOVE 2 TO OPTION-NEXT-NODE(1, 1)
           MOVE "NONE" TO OPTION-CONDITION(1, 1)
           MOVE "NONE" TO OPTION-ACTION(1, 1)
           MOVE "Que savez-vous sur la cité futuriste?" 
                TO OPTION-TEXT(1, 2)
           MOVE 3 TO OPTION-NEXT-NODE(1, 2)
           MOVE "NONE" TO OPTION-CONDITION(1, 2)
           MOVE "ADD_QUEST_INFO" TO OPTION-ACTION(1, 2)
           MOVE "Je dois y aller, au revoir." 
                TO OPTION-TEXT(1, 3)
           MOVE 0 TO OPTION-NEXT-NODE(1, 3)
           MOVE "NONE" TO OPTION-CONDITION(1, 3)
           MOVE "END_DIALOG" TO OPTION-ACTION(1, 3)
      
           MOVE 2 TO NODE-ID(2)
           MOVE 1 TO NODE-DIALOG-ID(2)
           MOVE "La Bibliothèque Centrale contient toute l'histoire "
                "de notre monde, depuis les premiers jours des cartes "
                "perforées jusqu'à l'ère actuelle. Chaque ligne de code "
                "qui a façonné MAINFRAME-TERRA est archivée ici." 
                TO NODE-TEXT(2)
           MOVE "Archiviste Ada" TO NODE-SPEAKER(2)
           MOVE 2 TO NODE-OPTIONS-COUNT(2)
           MOVE "Puis-je consulter ces archives?" 
                TO OPTION-TEXT(2, 1)
           MOVE 4 TO OPTION-NEXT-NODE(2, 1)
           MOVE "NONE" TO OPTION-CONDITION(2, 1)
           MOVE "INCREASE_REPUTATION" TO OPTION-ACTION(2, 1)
           MOVE "J'ai d'autres questions..." 
                TO OPTION-TEXT(2, 2)
           MOVE 1 TO OPTION-NEXT-NODE(2, 2)
           MOVE "NONE" TO OPTION-CONDITION(2, 2)
           MOVE "NONE" TO OPTION-ACTION(2, 2)
           
           MOVE 3 TO NODE-ID(3)
           MOVE 1 TO NODE-DIALOG-ID(3)
           MOVE "La cité futuriste... Un sujet fascinant. Elle semble "
                "exister en dehors de notre continuum temporel normal. "
                "Certains textes suggèrent qu'elle pourrait être "
                "le cœur même de MAINFRAME-TERRA, son origine." 
                TO NODE-TEXT(3)
           MOVE "Archiviste Ada" TO NODE-SPEAKER(3)
           MOVE 2 TO NODE-OPTIONS-COUNT(3)
           MOVE "Comment puis-je y accéder?" 
                TO OPTION-TEXT(3, 1)
           MOVE 5 TO OPTION-NEXT-NODE(3, 1)
           MOVE "NONE" TO OPTION-CONDITION(3, 1)
           MOVE "ADD_QUEST_OBJECTIVE" TO OPTION-ACTION(3, 1)
           MOVE "J'ai d'autres questions..." 
                TO OPTION-TEXT(3, 2)
           MOVE 1 TO OPTION-NEXT-NODE(3, 2)
           MOVE "NONE" TO OPTION-CONDITION(3, 2)
           MOVE "NONE" TO OPTION-ACTION(3, 2)
      
           MOVE 4 TO NODE-ID(4)
           MOVE 1 TO NODE-DIALOG-ID(4)
           MOVE "Bien sûr! Pour quelqu'un comme vous, je peux faire "
                "une exception. Voici un badge d'accès qui vous "
                "permettra d'explorer certaines sections. Revenez me "
                "voir si vous trouvez des fragments de code intéressants." 
                TO NODE-TEXT(4)
           MOVE "Archiviste Ada" TO NODE-SPEAKER(4)
           MOVE 1 TO NODE-OPTIONS-COUNT(4)
           MOVE "Merci pour votre aide." 
                TO OPTION-TEXT(4, 1)
           MOVE 1 TO OPTION-NEXT-NODE(4, 1)
           MOVE "NONE" TO OPTION-CONDITION(4, 1)
           MOVE "RECEIVE_ITEM" TO OPTION-ACTION(4, 1)
      
           MOVE 5 TO NODE-ID(5)
           MOVE 1 TO NODE-DIALOG-ID(5)
           MOVE "Il existe un gardien à l'entrée de la cité. "
                "On dit qu'il ne laisse passer que ceux qui comprennent "
                "véritablement le langage ancien. Cherchez Neumann à "
                "l'entrée de la cité. Mais soyez prudent, le chemin "
                "est semé d'embûches." 
                TO NODE-TEXT(5)
           MOVE "Archiviste Ada" TO NODE-SPEAKER(5)
           MOVE 2 TO NODE-OPTIONS-COUNT(5)
           MOVE "Je trouverai ce gardien." 
                TO OPTION-TEXT(5, 1)
           MOVE 1 TO OPTION-NEXT-NODE(5, 1)
           MOVE "NONE" TO OPTION-CONDITION(5, 1)
           MOVE "ACTIVATE_QUEST" TO OPTION-ACTION(5, 1)
           MOVE "Je reviendrai quand je serai prêt." 
                TO OPTION-TEXT(5, 2)
           MOVE 0 TO OPTION-NEXT-NODE(5, 2)
           MOVE "NONE" TO OPTION-CONDITION(5, 2)
           MOVE "END_DIALOG" TO OPTION-ACTION(5, 2)
      
           *> Dialogue 2: Technicien Turing
           MOVE 6 TO NODE-ID(6)
           MOVE 2 TO NODE-DIALOG-ID(6)
           MOVE "*bruit de cliquetis* Oh! Vous m'avez surpris. "
                "Je ne reçois pas souvent de visiteurs ici. "
                "Je suis Turing, technicien en chef de cette "
                "section. Que puis-je faire pour vous?" 
                TO NODE-TEXT(6)
           MOVE "Technicien Turing" TO NODE-SPEAKER(6)
           MOVE 3 TO NODE-OPTIONS-COUNT(6)
           MOVE "Que faites-vous ici?" 
                TO OPTION-TEXT(6, 1)
           MOVE 7 TO OPTION-NEXT-NODE(6, 1)
           MOVE "NONE" TO OPTION-CONDITION(6, 1)
           MOVE "NONE" TO OPTION-ACTION(6, 1)
           MOVE "J'ai trouvé cet étrange composant..." 
                TO OPTION-TEXT(6, 2)
           MOVE 8 TO OPTION-NEXT-NODE(6, 2)
           MOVE "HAS_COMPONENT" TO OPTION-CONDITION(6, 2)
           MOVE "SHOW_ITEM" TO OPTION-ACTION(6, 2)
           MOVE "Je ne faisais que passer." 
                TO OPTION-TEXT(6, 3)
           MOVE 0 TO OPTION-NEXT-NODE(6, 3)
           MOVE "NONE" TO OPTION-CONDITION(6, 3)
           MOVE "END_DIALOG" TO OPTION-ACTION(6, 3)
      
           MOVE 7 TO NODE-ID(7)
           MOVE 2 TO NODE-DIALOG-ID(7)
           MOVE "Je maintiens les systèmes en état de fonctionnement. "
                "Ces machines sont anciennes mais essentielles. "
                "Elles contiennent le code source original qui fait "
                "fonctionner notre monde. Sans maintenance constante, "
                "tout pourrait... disparaître." 
                TO NODE-TEXT(7)
           MOVE "Technicien Turing" TO NODE-SPEAKER(7)
           MOVE 2 TO NODE-OPTIONS-COUNT(7)
           MOVE "Avez-vous besoin d'aide?" 
                TO OPTION-TEXT(7, 1)
           MOVE 9 TO OPTION-NEXT-NODE(7, 1)
           MOVE "NONE" TO OPTION-CONDITION(7, 1)
           MOVE "OFFER_QUEST" TO OPTION-ACTION(7, 1)
           MOVE "Intéressant. Je dois y aller." 
                TO OPTION-TEXT(7, 2)
           MOVE 0 TO OPTION-NEXT-NODE(7, 2)
           MOVE "NONE" TO OPTION-CONDITION(7, 2)
           MOVE "END_DIALOG" TO OPTION-ACTION(7, 2)
      
           MOVE 8 TO NODE-ID(8)
           MOVE 2 TO NODE-DIALOG-ID(8)
           MOVE "Par le grand compilateur! C'est un module d'extension "
                "temporelle! Où avez-vous trouvé cela? Ces composants "
                "sont extrêmement rares. Il pourrait nous aider à "
                "stabiliser les fluctuations que nous observons "
                "dernièrement." 
                TO NODE-TEXT(8)
           MOVE "Technicien Turing" TO NODE-SPEAKER(8)
           MOVE 2 TO NODE-OPTIONS-COUNT(8)
           MOVE "Vous pouvez le garder si cela aide." 
                TO OPTION-TEXT(8, 1)
           MOVE 9 TO OPTION-NEXT-NODE(8, 1)
           MOVE "NONE" TO OPTION-CONDITION(8, 1)
           MOVE "GIVE_ITEM" TO OPTION-ACTION(8, 1)
           MOVE "Je préfère le conserver pour l'instant." 
                TO OPTION-TEXT(8, 2)
           MOVE 6 TO OPTION-NEXT-NODE(8, 2)
           MOVE "NONE" TO OPTION-CONDITION(8, 2)
           MOVE "DECREASE_REPUTATION" TO OPTION-ACTION(8, 2)
      
           MOVE 9 TO NODE-ID(9)
           MOVE 2 TO NODE-DIALOG-ID(9)
           MOVE "Votre aide est inestimable! En remerciement, laissez-moi "
                "vous donner accès au terminal temporel. Il est encore "
                "expérimental, mais il pourrait vous permettre d'explorer "
                "différentes époques de notre histoire informatique. "
                "Cela pourrait être utile dans votre quête." 
                TO NODE-TEXT(9)
           MOVE "Technicien Turing" TO NODE-SPEAKER(9)
           MOVE 2 TO NODE-OPTIONS-COUNT(9)
           MOVE "Comment fonctionne ce terminal?" 
                TO OPTION-TEXT(9, 1)
           MOVE 6 TO OPTION-NEXT-NODE(9, 1)
           MOVE "NONE" TO OPTION-CONDITION(9, 1)
           MOVE "UNLOCK_TIME_TRAVEL" TO OPTION-ACTION(9, 1)
           MOVE "Merci! Je reviendrai bientôt." 
                TO OPTION-TEXT(9, 2)
           MOVE 0 TO OPTION-NEXT-NODE(9, 2)
           MOVE "NONE" TO OPTION-CONDITION(9, 2)
           MOVE "END_DIALOG,UNLOCK_TIME_TRAVEL" TO OPTION-ACTION(9, 2)
      
           *> Dialogue 3: Gardien Neumann
           MOVE 10 TO NODE-ID(10)
           MOVE 3 TO NODE-DIALOG-ID(10)
           MOVE "Halte! Je suis Neumann, gardien de cette entrée. "
                "Personne ne peut passer sans démontrer sa compréhension "
                "du langage ancien. Êtes-vous prêt à relever le défi?" 
                TO NODE-TEXT(10)
           MOVE "Gardien Neumann" TO NODE-SPEAKER(10)
           MOVE 2 TO NODE-OPTIONS-COUNT(10)
           MOVE "Je suis prêt. Quel est ce défi?" 
                TO OPTION-TEXT(10, 1)
           MOVE 11 TO OPTION-NEXT-NODE(10, 1)
           MOVE "NONE" TO OPTION-CONDITION(10, 1)
           MOVE "NONE" TO OPTION-ACTION(10, 1)
           MOVE "Je reviendrai quand je serai mieux préparé." 
                TO OPTION-TEXT(10, 2)
           MOVE 0 TO OPTION-NEXT-NODE(10, 2)
           MOVE "NONE" TO OPTION-CONDITION(10, 2)
           MOVE "END_DIALOG" TO OPTION-ACTION(10, 2)
      
           MOVE 11 TO NODE-ID(11)
           MOVE 3 TO NODE-DIALOG-ID(11)
           MOVE "Très bien. Vous devez compléter cette instruction COBOL: "
                "'COMPUTE RESULT = X * Y / Z + ...'. "
                "Quelle est la priorité d'opération correcte dans COBOL?" 
                TO NODE-TEXT(11)
           MOVE "Gardien Neumann" TO NODE-SPEAKER(11)
           MOVE 3 TO NODE-OPTIONS-COUNT(11)
           MOVE "De gauche à droite, strictement." 
                TO OPTION-TEXT(11, 1)
           MOVE 12 TO OPTION-NEXT-NODE(11, 1)
           MOVE "NONE" TO OPTION-CONDITION(11, 1)
           MOVE "WRONG_ANSWER" TO OPTION-ACTION(11, 1)
           MOVE "Multiplication/division d'abord, puis addition/soustraction." 
                TO OPTION-TEXT(11, 2)
           MOVE 12 TO OPTION-NEXT-NODE(11, 2)
           MOVE "NONE" TO OPTION-CONDITION(11, 2)
           MOVE "CORRECT_ANSWER" TO OPTION-ACTION(11, 2)
           MOVE "Comme en mathématiques standard: parenthèses, exposants, etc." 
                TO OPTION-TEXT(11, 3)
           MOVE 12 TO OPTION-NEXT-NODE(11, 3)
           MOVE "NONE" TO OPTION-CONDITION(11, 3)
           MOVE "WRONG_ANSWER" TO OPTION-ACTION(11, 3)
      
           MOVE 12 TO NODE-ID(12)
           MOVE 3 TO NODE-DIALOG-ID(12)
           MOVE "CONDITION_TEXT" 
                TO NODE-TEXT(12)
           MOVE "Gardien Neumann" TO NODE-SPEAKER(12)
           MOVE 1 TO NODE-OPTIONS-COUNT(12)
           MOVE "[Continuer]" 
                TO OPTION-TEXT(12, 1)
           MOVE 0 TO OPTION-NEXT-NODE(12, 1)
           MOVE "NONE" TO OPTION-CONDITION(12, 1)
           MOVE "END_DIALOG" TO OPTION-ACTION(12, 1)
      
           *> Dialogue 4: Voyageur Temporel
           MOVE 13 TO NODE-ID(13)
           MOVE 4 TO NODE-DIALOG-ID(13)
           MOVE "*apparaît soudainement* Ah! Vous voilà enfin. "
                "Je vous attendais... ou vous attendrai... "
                "le temps est si confus quand on voyage à travers. "
                "Je suis vous, d'une certaine façon, mais d'un futur "
                "qui n'existe peut-être plus." 
                TO NODE-TEXT(13)
           MOVE "Voyageur Temporel" TO NODE-SPEAKER(13)
           MOVE 3 TO NODE-OPTIONS-COUNT(13)
           MOVE "Que voulez-vous dire? Vous êtes moi?" 
                TO OPTION-TEXT(13, 1)
           MOVE 14 TO OPTION-NEXT-NODE(13, 1)
           MOVE "NONE" TO OPTION-CONDITION(13, 1)
           MOVE "NONE" TO OPTION-ACTION(13, 1)
           MOVE "Pourquoi êtes-vous ici?" 
                TO OPTION-TEXT(13, 2)
           MOVE 15 TO OPTION-NEXT-NODE(13, 2)
           MOVE "NONE" TO OPTION-CONDITION(13, 2)
           MOVE "NONE" TO OPTION-ACTION(13, 2)
           MOVE "Je n'ai pas temps pour ces absurdités." 
                TO OPTION-TEXT(13, 3)
           MOVE 0 TO OPTION-NEXT-NODE(13, 3)
           MOVE "NONE" TO OPTION-CONDITION(13, 3)
           MOVE "END_DIALOG,DECREASE_REPUTATION" TO OPTION-ACTION(13, 3)
      
           MOVE 14 TO NODE-ID(14)
           MOVE 4 TO NODE-DIALOG-ID(14)
           MOVE "Disons que nous partageons le même code source, mais "
                "des versions différentes. Je viens d'un avenir où nos "
                "choix ont conduit à une corruption irréparable du système. "
                "Je suis revenu pour vous aider à éviter ce destin." 
                TO NODE-TEXT(14)
           MOVE "Voyageur Temporel" TO NODE-SPEAKER(14)
           MOVE 1 TO NODE-OPTIONS-COUNT(14)
           MOVE "Comment puis-je éviter cette catastrophe?" 
                TO OPTION-TEXT(14, 1)
           MOVE 15 TO OPTION-NEXT-NODE(14, 1)
           MOVE "NONE" TO OPTION-CONDITION(14, 1)
           MOVE "NONE" TO OPTION-ACTION(14, 1)
      
           MOVE 15 TO NODE-ID(15)
           MOVE 4 TO NODE-DIALOG-ID(15)
           MOVE "Vous devez trouver et réparer les anomalies temporelles "
                "à travers les différentes époques. Utilisez le terminal "
                "temporel, explorez notre histoire, et collectez les "
                "fragments dispersés du code source original. C'est notre "
                "seul espoir de stabiliser MAINFRAME-TERRA." 
                TO NODE-TEXT(15)
           MOVE "Voyageur Temporel" TO NODE-SPEAKER(15)
           MOVE 2 TO NODE-OPTIONS-COUNT(15)
           MOVE "Je ferai de mon mieux." 
                TO OPTION-TEXT(15, 1)
           MOVE 0 TO OPTION-NEXT-NODE(15, 1)
           MOVE "NONE" TO OPTION-CONDITION(15, 1)
           MOVE "END_DIALOG,ADD_TIME_QUEST" TO OPTION-ACTION(15, 1)
           MOVE "Avez-vous d'autres conseils?" 
                TO OPTION-TEXT(15, 2)
           MOVE 0 TO OPTION-NEXT-NODE(15, 2)
           MOVE "NONE" TO OPTION-CONDITION(15, 2)
           MOVE "END_DIALOG,ADD_TIME_QUEST,GIVE_HINT" TO OPTION-ACTION(15, 2)
           .
      
      *-----------------------------------------------------------------
      * Démarrage d'un dialogue avec un PNJ spécifique
      *-----------------------------------------------------------------
       START-DIALOG.
           *> Paramètre: DIALOG-ID-TO-START
      
           MOVE "N" TO NODE-FOUND
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DIALOG-COUNT
               IF DIALOG-ID(I) = DIALOG-ID-TO-START
                   MOVE DIALOG-ID-TO-START TO CURRENT-DIALOG-ID
                   MOVE DIALOG-ROOT-NODE(I) TO CURRENT-NODE-ID
                   MOVE DIALOG-NPC-NAME(I) TO SPEAKER-NAME
                   MOVE "Y" TO DIALOG-ACTIVE
                   MOVE "Y" TO NODE-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM
      
           IF NODE-FOUND = "Y"
               PERFORM DISPLAY-DIALOG-NODE
           ELSE
               DISPLAY "Erreur: Dialogue non trouvé!"
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Affichage d'un nœud de dialogue
      *-----------------------------------------------------------------
       DISPLAY-DIALOG-NODE.
           PERFORM FIND-CURRENT-NODE
      
           DISPLAY SPACE
           DISPLAY "--- " NODE-SPEAKER(NODE-INDEX) " ---"
           DISPLAY SPACE
      
           *> Afficher le texte du dialogue avec retour à la ligne
           MOVE NODE-TEXT(NODE-INDEX) TO TEXT-TO-WRAP
           MOVE DISPLAY-WIDTH TO WRAP-WIDTH
           PERFORM WRAP-TEXT
      
           *> Afficher les options de dialogue disponibles
           DISPLAY SPACE
           MOVE 0 TO VALID-OPTIONS-COUNT
      
           PERFORM VARYING I FROM 1 BY 1 
                   UNTIL I > NODE-OPTIONS-COUNT(NODE-INDEX)
               MOVE "Y" TO OPTION-IS-VALID
      
               *> Vérifier les conditions pour cette option
               IF OPTION-CONDITION(NODE-INDEX, I) NOT = "NONE"
                   PERFORM CHECK-DIALOG-CONDITION
                   IF CONDITION-MET = "N"
                       MOVE "N" TO OPTION-IS-VALID
                   END-IF
               END-IF
      
               IF OPTION-IS-VALID = "Y"
                   ADD 1 TO VALID-OPTIONS-COUNT
                   MOVE VALID-OPTIONS-COUNT TO DISPLAY-INDEX
                   DISPLAY DISPLAY-INDEX ". " OPTION-TEXT(NODE-INDEX, I)
               END-IF
           END-PERFORM
      
           *> Demander le choix du joueur
           IF VALID-OPTIONS-COUNT > 0
               DISPLAY SPACE
               DISPLAY "Votre choix: " WITH NO ADVANCING
               ACCEPT DIALOG-CHOICE
      
               IF DIALOG-CHOICE > 0 AND 
                  DIALOG-CHOICE <= VALID-OPTIONS-COUNT
                   PERFORM PROCESS-DIALOG-CHOICE
               ELSE
                   DISPLAY "Choix invalide, veuillez réessayer."
                   PERFORM DISPLAY-DIALOG-NODE
               END-IF
           ELSE
               DISPLAY "Ce PNJ n'a rien d'autre à dire pour le moment."
               MOVE "N" TO DIALOG-ACTIVE
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Recherche du nœud courant
      *-----------------------------------------------------------------
       FIND-CURRENT-NODE.
           MOVE 1 TO NODE-INDEX
      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODE-COUNT
               IF NODE-ID(I) = CURRENT-NODE-ID AND
                  NODE-DIALOG-ID(I) = CURRENT-DIALOG-ID
                   MOVE I TO NODE-INDEX
                   EXIT PERFORM
               END-IF
           END-PERFORM
      
           *> Vérifier si le texte du nœud a besoin d'être conditionnel
           IF NODE-TEXT(NODE-INDEX) = "CONDITION_TEXT"
               *> Cas spécial pour le gardien Neumann
               IF NODE-ID(NODE-INDEX) = 12
                   IF LAST-ACTION = "CORRECT_ANSWER"
                       MOVE "Impressionnant! Vous connaissez bien le langage "
                            "ancestral. L'entrée de la cité vous est ouverte. "
                            "Soyez prudent, car la connaissance qui s'y trouve "
                            "pourrait changer votre perception de la réalité."
                            TO NODE-TEXT(NODE-INDEX)
                       MOVE "COMPLETE_QUEST_OBJECTIVE" TO NEXT-ACTION
                   ELSE
                       MOVE "Incorrect! Vous n'êtes pas encore prêt à entrer "
                            "dans la cité. Étudiez le langage ancien et "
                            "revenez quand vous maîtriserez ses règles "
                            "fondamentales."
                            TO NODE-TEXT(NODE-INDEX)
                   END-IF
               END-IF
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Traitement du choix de dialogue
      *-----------------------------------------------------------------
       PROCESS-DIALOG-CHOICE.
           MOVE 0 TO VALID-INDEX
           MOVE 0 TO VALID-COUNT
      
           *> Trouver l'option sélectionnée (en tenant compte des filtres)
           PERFORM VARYING I FROM 1 BY 1 
                   UNTIL I > NODE-OPTIONS-COUNT(NODE-INDEX)
               MOVE "Y" TO OPTION-IS-VALID
      
               IF OPTION-CONDITION(NODE-INDEX, I) NOT = "NONE"
                   PERFORM CHECK-DIALOG-CONDITION
                   IF CONDITION-MET = "N"
                       MOVE "N" TO OPTION-IS-VALID
                   END-IF
               END-IF
      
               IF OPTION-IS-VALID = "Y"
                   ADD 1 TO VALID-COUNT
                   IF VALID-COUNT = DIALOG-CHOICE
                       MOVE I TO VALID-INDEX
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM
      
           *> Exécuter les actions associées au choix
           IF OPTION-ACTION(NODE-INDEX, VALID-INDEX) NOT = "NONE"
               MOVE OPTION-ACTION(NODE-INDEX, VALID-INDEX) TO ACTION-LIST
               PERFORM PROCESS-DIALOG-ACTIONS
           END-IF
      
           *> Passer au nœud suivant ou terminer le dialogue
           IF OPTION-NEXT-NODE(NODE-INDEX, VALID-INDEX) > 0
               MOVE OPTION-NEXT-NODE(NODE-INDEX, VALID-INDEX) 
                    TO CURRENT-NODE-ID
               PERFORM DISPLAY-DIALOG-NODE
           ELSE
               MOVE "N" TO DIALOG-ACTIVE
               DISPLAY SPACE
               DISPLAY "Fin du dialogue."
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Vérification d'une condition de dialogue
      *-----------------------------------------------------------------
       CHECK-DIALOG-CONDITION.
           MOVE "N" TO CONDITION-MET
      
           EVALUATE OPTION-CONDITION(NODE-INDEX, I)
               WHEN "HAS_COMPONENT"
                   *> Vérifier si le joueur a le composant dans l'inventaire
                   *> Dans un vrai jeu, cela interrogerait le système d'inventaire
                   MOVE "Y" TO CONDITION-MET
               WHEN "QUEST_COMPLETED"
                   IF NPC-QUEST-COMPLETED = "Y"
                       MOVE "Y" TO CONDITION-MET
                   END-IF
               WHEN "HIGH_REPUTATION"
                   IF NPC-RELATIONSHIP > 70
                       MOVE "Y" TO CONDITION-MET
                   END-IF
               WHEN "NONE"
                   MOVE "Y" TO CONDITION-MET
               WHEN OTHER
                   MOVE "N" TO CONDITION-MET
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Exécution des actions de dialogue
      *-----------------------------------------------------------------
       PROCESS-DIALOG-ACTIONS.
           *> Les actions peuvent être séparées par des virgules
           PERFORM PARSE-ACTION-LIST
      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACTION-COUNT
               MOVE ACTION-ITEM(I) TO CURRENT-ACTION
               PERFORM EXECUTE-DIALOG-ACTION
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Analyse de la liste d'actions
      *-----------------------------------------------------------------
       PARSE-ACTION-LIST.
           MOVE 0 TO ACTION-COUNT
           MOVE 1 TO TEXT-POS
           MOVE 1 TO CURRENT-ACTION-INDEX
      
           PERFORM UNTIL TEXT-POS > FUNCTION LENGTH(ACTION-LIST)
               IF ACTION-LIST(TEXT-POS:1) = ","
                   ADD 1 TO ACTION-COUNT
                   ADD 1 TO CURRENT-ACTION-INDEX
                   ADD 1 TO TEXT-POS
               ELSE
                   MOVE ACTION-LIST(TEXT-POS:1) 
                        TO ACTION-ITEM(ACTION-COUNT)(CURRENT-ACTION-INDEX:1)
                   ADD 1 TO CURRENT-ACTION-INDEX
                   ADD 1 TO TEXT-POS
               END-IF
           END-PERFORM
           .
      
      *-----------------------------------------------------------------
      * Exécution d'une action de dialogue spécifique
      *-----------------------------------------------------------------
       EXECUTE-DIALOG-ACTION.
           EVALUATE CURRENT-ACTION
               WHEN "END_DIALOG"
                   MOVE "N" TO DIALOG-ACTIVE
               WHEN "INCREASE_REPUTATION"
                   ADD 10 TO NPC-RELATIONSHIP
                   DISPLAY "(Votre relation avec ce PNJ s'est améliorée.)"
               WHEN "DECREASE_REPUTATION"
                   SUBTRACT 10 FROM NPC-RELATIONSHIP
                   DISPLAY "(Votre relation avec ce PNJ s'est détériorée.)"
               WHEN "ACTIVATE_QUEST"
                   MOVE "Y" TO NPC-QUEST-OFFERED
                   DISPLAY "(Nouvelle quête ajoutée au journal!)"
               WHEN "RECEIVE_ITEM"
                   DISPLAY "(Vous avez reçu: Badge d'accès bibliothèque)"
               WHEN "GIVE_ITEM"
                   DISPLAY "(Vous avez donné: Module d'extension temporelle)"
               WHEN "UNLOCK_TIME_TRAVEL"
                   DISPLAY "(Vous avez maintenant accès au Terminal Time Travel!)"
               WHEN "ADD_QUEST_INFO"
                   DISPLAY "(Informations ajoutées au journal de quêtes.)"
               WHEN "ADD_QUEST_OBJECTIVE"
                   DISPLAY "(Nouvel objectif de quête ajouté.)"
               WHEN "COMPLETE_QUEST_OBJECTIVE"
                   DISPLAY "(Objectif de quête accompli!)"
               WHEN "SHOW_ITEM"
                   DISPLAY "(Vous montrez l'objet.)"
               WHEN "CORRECT_ANSWER"
                   MOVE "CORRECT_ANSWER" TO LAST-ACTION
               WHEN "WRONG_ANSWER"
                   MOVE "WRONG_ANSWER" TO LAST-ACTION
               WHEN "ADD_TIME_QUEST"
                   DISPLAY "(Quête temporelle ajoutée au journal!)"
               WHEN "GIVE_HINT"
                   DISPLAY "(Indice: Cherchez les anomalies temporelles dans les époques les plus anciennes d'abord.)"
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Formatage du texte avec retour à la ligne
      *-----------------------------------------------------------------
       WRAP-TEXT.
           MOVE 1 TO TEXT-POS
           MOVE FUNCTION LENGTH(TEXT-TO-WRAP) TO TEXT-LENGTH
      
           PERFORM UNTIL TEXT-POS > TEXT-LENGTH
               MOVE 0 TO CHAR-COUNTER
               MOVE SPACES TO TEXT-PART
      
               *> Extraire un segment de texte selon la largeur
               PERFORM UNTIL CHAR-COUNTER >= WRAP-WIDTH OR 
                             TEXT-POS > TEXT-LENGTH
                   MOVE TEXT-TO-WRAP(TEXT-POS:1) TO TEXT-PART(CHAR-COUNTER + 1:1)
                   ADD 1 TO TEXT-POS
                   ADD 1 TO CHAR-COUNTER
               END-PERFORM
      
               *> Afficher la ligne
               DISPLAY TEXT-PART
           END-PERFORM
           .
      
       END PROGRAM DIALOG-MULTIPLEXER.