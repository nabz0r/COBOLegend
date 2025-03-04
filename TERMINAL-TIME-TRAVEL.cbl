      *****************************************************************
      * TERMINAL-TIME-TRAVEL.CBL - Système de voyage temporel pour COBOLegend
      *
      * Ce module permet aux joueurs d'explorer différentes ères
      * de l'histoire informatique à travers des Chronoterminaux.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TERMINAL-TIME-TRAVEL.
       AUTHOR. CLAUDE.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Structure de données pour les voyages temporels
      *-----------------------------------------------------------------
       01 TIME-TRAVEL-SYSTEM.
          05 CURRENT-ERA             PIC 9(1)   VALUE 3.
          05 ERA-UNLOCKED OCCURS 5 TIMES.
             10 ERA-ID               PIC 9(1).
             10 ERA-NAME             PIC X(30).
             10 ERA-ACCESS-STATUS    PIC X(1).
                88 ERA-AVAILABLE     VALUE 'Y'.
                88 ERA-LOCKED        VALUE 'N'.
             10 ERA-COMPLETION       PIC 9(3)   VALUE 0.
          05 TEMPORAL-STABILITY      PIC 9(3)   VALUE 100.
          05 TIME-ARTIFACTS-COLLECTED PIC 9(2)  VALUE 0.
      
      *-----------------------------------------------------------------
      * Définition des ères informatiques
      *-----------------------------------------------------------------
       01 COMPUTER-ERAS.
          05 ERA-1-DETAILS.
             10 ERA-1-ID             PIC 9(1)   VALUE 1.
             10 ERA-1-NAME           PIC X(30)  VALUE
                "Ere des Cartes Perforees (1950-1960)".
             10 ERA-1-DESC           PIC X(255) VALUE
                "Une époque où les programmeurs communiquaient avec "
                "les machines par l'intermédiaire de cartes perforées. "
                "Les salles informatiques bourdonnent du son mécanique "
                "des lecteurs de cartes et des imprimantes à papier "
                "continu. Les calculs prennent des heures ou des jours.".
          05 ERA-2-DETAILS.
             10 ERA-2-ID             PIC 9(1)   VALUE 2.
             10 ERA-2-NAME           PIC X(30)  VALUE
                "Ere des Mainframes (1960-1970)".
             10 ERA-2-DESC           PIC X(255) VALUE
                "L'époque d'or des mainframes, où COBOL règne en "
                "maître. D'énormes ordinateurs occupent des salles "
                "entières, refroidies par des systèmes sophistiqués. "
                "Les premiers terminaux permettent d'interagir "
                "directement avec le système central.".
          05 ERA-3-DETAILS.
             10 ERA-3-ID             PIC 9(1)   VALUE 3.
             10 ERA-3-NAME           PIC X(30)  VALUE
                "Ere Microinformatique (1980-1990)".
             10 ERA-3-DESC           PIC X(255) VALUE
                "L'émergence des ordinateurs personnels transforme "
                "le paysage informatique. Les grands mainframes "
                "coexistent avec des micro-ordinateurs de plus en "
                "plus puissants. Les interfaces graphiques "
                "commencent à remplacer les lignes de commande.".
          05 ERA-4-DETAILS.
             10 ERA-4-ID             PIC 9(1)   VALUE 4.
             10 ERA-4-NAME           PIC X(30)  VALUE
                "Ere Internet (1990-2000)".
             10 ERA-4-DESC           PIC X(255) VALUE
                "La révolution d'Internet connecte les systèmes du "
                "monde entier. Les mainframes COBOL doivent s'adapter "
                "pour communiquer avec le web émergent. Les navigateurs "
                "ouvrent de nouvelles possibilités d'interaction entre "
                "les humains et les machines.".
          05 ERA-5-DETAILS.
             10 ERA-5-ID             PIC 9(1)   VALUE 5.
             10 ERA-5-NAME           PIC X(30)  VALUE
                "Ere Cloud (2010-2020)".
             10 ERA-5-DESC           PIC X(255) VALUE
                "L'époque de la virtualisation et des services cloud. "
                "Les mainframes physiques sont souvent virtualisés, "
                "mais les systèmes COBOL demeurent critiques pour de "
                "nombreuses infrastructures. Le défi est de faire "
                "cohabiter l'ancien et le nouveau.".
      
      *-----------------------------------------------------------------
      * Objets spéciaux de chaque ère
      *-----------------------------------------------------------------
       01 ERA-ARTIFACTS.
          05 ERA-1-ARTIFACTS.
             10 ARTIFACT-1-1         PIC X(30)  VALUE
                "Perforateur de precision".
             10 ARTIFACT-1-2         PIC X(30)  VALUE
                "Cartes modele IBM".
          05 ERA-2-ARTIFACTS.
             10 ARTIFACT-2-1         PIC X(30)  VALUE
                "Bande magnetique encodee".
             10 ARTIFACT-2-2         PIC X(30)  VALUE
                "Manuel COBOL original".
          05 ERA-3-ARTIFACTS.
             10 ARTIFACT-3-1         PIC X(30)  VALUE
                "Disquette de demarrage".
             10 ARTIFACT-3-2         PIC X(30)  VALUE
                "Processeur 8-bit portatif".
          05 ERA-4-ARTIFACTS.
             10 ARTIFACT-4-1         PIC X(30)  VALUE
                "Routeur prototype".
             10 ARTIFACT-4-2         PIC X(30)  VALUE
                "Disque du premier navigateur".
          05 ERA-5-ARTIFACTS.
             10 ARTIFACT-5-1         PIC X(30)  VALUE
                "Jeton d'authentification quantique".
             10 ARTIFACT-5-2         PIC X(30)  VALUE
                "Conteneur virtuel".
      
      *-----------------------------------------------------------------
      * Variables pour les défis et quêtes temporelles
      *-----------------------------------------------------------------
       01 TEMPORAL-QUESTS.
          05 MAIN-QUEST-1            PIC X(50)  VALUE
             "Recuperation du code source perdu".
          05 MAIN-QUEST-2            PIC X(50)  VALUE
             "Resolution de l'anomalie temporelle".
          05 QUEST-PROGRESS-1        PIC 9(3)   VALUE 0.
          05 QUEST-PROGRESS-2        PIC 9(3)   VALUE 0.
      
       01 TEMPORAL-ANOMALIES.
          05 ANOMALY-COUNT           PIC 9(2)   VALUE 3.
          05 ANOMALY-FIXED-COUNT     PIC 9(2)   VALUE 0.
          05 ANOMALY-LOCATIONS.
             10 ANOMALY-ERA OCCURS 3 TIMES PIC 9(1).
             10 ANOMALY-DESC OCCURS 3 TIMES PIC X(100).
             10 ANOMALY-FIXED OCCURS 3 TIMES PIC X(1).
                88 ANOMALY-IS-FIXED  VALUE 'Y'.
      
       01 CHRONOTERMINAL-STATUS      PIC X(1)   VALUE 'A'.
          88 TERMINAL-ACTIVE         VALUE 'A'.
          88 TERMINAL-INACTIVE       VALUE 'I'.
          88 TERMINAL-UNSTABLE       VALUE 'U'.
      
       01 INTERFACE-VARIABLES.
          05 TERMINAL-CHOICE         PIC 9(1)   VALUE 0.
          05 ERA-CHOICE              PIC 9(1)   VALUE 0.
          05 ACTION-CHOICE           PIC 9(1)   VALUE 0.
          05 CONFIRMATION            PIC X(1)   VALUE 'N'.
      
       01 TEMPORAL-EFFECTS.
          05 TIME-DRIFT              PIC 9(3)   VALUE 0.
          05 PARADOX-RISK            PIC 9(3)   VALUE 0.
          05 STABILITY-THRESHOLD     PIC 9(3)   VALUE 30.
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation du système de voyage temporel
      *-----------------------------------------------------------------
       INITIALIZE-TIME-TRAVEL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               MOVE I TO ERA-ID(I)
      
               EVALUATE I
                   WHEN 1
                       MOVE ERA-1-NAME TO ERA-NAME(I)
                   WHEN 2
                       MOVE ERA-2-NAME TO ERA-NAME(I)
                   WHEN 3
                       MOVE ERA-3-NAME TO ERA-NAME(I)
                   WHEN 4
                       MOVE ERA-4-NAME TO ERA-NAME(I)
                   WHEN 5
                       MOVE ERA-5-NAME TO ERA-NAME(I)
               END-EVALUATE
      
               IF I = 3
                   MOVE "Y" TO ERA-ACCESS-STATUS(I)
               ELSE
                   MOVE "N" TO ERA-ACCESS-STATUS(I)
               END-IF
           END-PERFORM
      
           MOVE 1 TO ANOMALY-ERA(1)
           MOVE 3 TO ANOMALY-ERA(2)
           MOVE 4 TO ANOMALY-ERA(3)
      
           MOVE "Instabilite dans le flux de donnees des cartes perforees"
               TO ANOMALY-DESC(1)
           MOVE "Corruption dans l'architecture de la memoire virtuelle"
               TO ANOMALY-DESC(2)
           MOVE "Interferences temporelles dans les protocoles reseau"
               TO ANOMALY-DESC(3)
      
           MOVE "N" TO ANOMALY-FIXED(1)
           MOVE "N" TO ANOMALY-FIXED(2)
           MOVE "N" TO ANOMALY-FIXED(3)
           .
      
      *-----------------------------------------------------------------
      * Menu principal du voyage temporel
      *-----------------------------------------------------------------
       CHRONOTERMINAL-MAIN-MENU.
           DISPLAY SPACE
           DISPLAY "***********************************************"
           DISPLAY "*              CHRONOTERMINAL                 *"
           DISPLAY "*   Portail vers les epoques informatiques   *"
           DISPLAY "***********************************************"
           DISPLAY SPACE
           DISPLAY "Stabilite temporelle: " TEMPORAL-STABILITY "%"
           DISPLAY "Artefacts collectes: " TIME-ARTIFACTS-COLLECTED
           DISPLAY SPACE
      
           DISPLAY "Que souhaitez-vous faire?"
           DISPLAY "1. Explorer une ere informatique"
           DISPLAY "2. Examiner les anomalies temporelles"
           DISPLAY "3. Consulter les artefacts collectes"
           DISPLAY "4. Verifier le statut des quetes temporelles"
           DISPLAY "5. Quitter le Chronoterminal"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT TERMINAL-CHOICE
      
           EVALUATE TERMINAL-CHOICE
               WHEN 1
                   PERFORM SELECT-ERA-MENU
               WHEN 2
                   PERFORM EXAMINE-ANOMALIES
               WHEN 3
                   PERFORM VIEW-ARTIFACTS
               WHEN 4
                   PERFORM CHECK-TEMPORAL-QUESTS
               WHEN 5
                   MOVE "I" TO CHRONOTERMINAL-STATUS
                   DISPLAY "Vous quittez le Chronoterminal."
               WHEN OTHER
                   DISPLAY "Option non reconnue."
                   PERFORM CHRONOTERMINAL-MAIN-MENU
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Sélection d'une ère à explorer
      *-----------------------------------------------------------------
       SELECT-ERA-MENU.
           DISPLAY SPACE
           DISPLAY "=== SELECTION D'ERE ==="
           DISPLAY "Choisissez une epoque a explorer:"
           DISPLAY SPACE
      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               IF ERA-ACCESS-STATUS(I) = "Y"
                   DISPLAY I ". " ERA-NAME(I) " (DISPONIBLE)"
               ELSE
                   DISPLAY I ". " ERA-NAME(I) " (VERROUILLE)"
               END-IF
           END-PERFORM
      
           DISPLAY "0. Retour"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT ERA-CHOICE
      
           IF ERA-CHOICE = 0
               PERFORM CHRONOTERMINAL-MAIN-MENU
           ELSE
               IF ERA-CHOICE > 0 AND ERA-CHOICE <= 5
                   IF ERA-ACCESS-STATUS(ERA-CHOICE) = "Y"
                       PERFORM ACTIVATE-CHRONOTERMINAL
                   ELSE
                       DISPLAY "Cette ere n'est pas encore disponible."
                       DISPLAY "Vous devez d'abord debloquer son acces."
                       DISPLAY SPACE
                       DISPLAY "Appuyez sur ENTREE pour continuer..."
                       ACCEPT CONFIRMATION
                       PERFORM SELECT-ERA-MENU
                   END-IF
               ELSE
                   DISPLAY "Option non reconnue."
                   PERFORM SELECT-ERA-MENU
               END-IF
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Activation du Chronoterminal pour voyage
      *-----------------------------------------------------------------
       ACTIVATE-CHRONOTERMINAL.
           COMPUTE TIME-DRIFT = FUNCTION RANDOM * 10
           COMPUTE PARADOX-RISK = 
               (100 - TEMPORAL-STABILITY) * FUNCTION RANDOM * 0.5
      
           DISPLAY SPACE
           DISPLAY "Initialisation du Chronoterminal..."
           DISPLAY "Calibration des flux temporels..."
           DISPLAY "Deviation temporelle calculee: " TIME-DRIFT "%"
           DISPLAY "Risque de paradoxe: " PARADOX-RISK "%"
      
           IF PARADOX-RISK > STABILITY-THRESHOLD
               DISPLAY SPACE
               DISPLAY "AVERTISSEMENT: Instabilite temporelle detectee!"
               DISPLAY "Le voyage pourrait entrainer des perturbations."
               DISPLAY "Continuer tout de meme? (O/N)"
               ACCEPT CONFIRMATION
      
               IF CONFIRMATION = "O" OR CONFIRMATION = "o"
                   PERFORM TRANSFER-PLAYER-TO-ERA
               ELSE
                   DISPLAY "Voyage annule par mesure de securite."
                   PERFORM SELECT-ERA-MENU
               END-IF
           ELSE
               PERFORM TRANSFER-PLAYER-TO-ERA
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Transfert du joueur dans l'ère sélectionnée
      *-----------------------------------------------------------------
       TRANSFER-PLAYER-TO-ERA.
           DISPLAY SPACE
           DISPLAY "Transfert en cours vers " ERA-NAME(ERA-CHOICE)
           DISPLAY "..."
           DISPLAY "..."
           DISPLAY "Transfert complete!"
           DISPLAY SPACE
      
           EVALUATE ERA-CHOICE
               WHEN 1
                   DISPLAY ERA-1-DESC
                   PERFORM ERA-1-INTERACTION
               WHEN 2
                   DISPLAY ERA-2-DESC
                   PERFORM ERA-2-INTERACTION
               WHEN 3
                   DISPLAY ERA-3-DESC
                   PERFORM ERA-3-INTERACTION
               WHEN 4
                   DISPLAY ERA-4-DESC
                   PERFORM ERA-4-INTERACTION
               WHEN 5
                   DISPLAY ERA-5-DESC
                   PERFORM ERA-5-INTERACTION
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Interaction avec l'ère des Cartes Perforées
      *-----------------------------------------------------------------
       ERA-1-INTERACTION.
           DISPLAY SPACE
           DISPLAY "Vous vous trouvez dans un laboratoire informatique"
           DISPLAY "des annees 1950. D'impressionnantes machines"
           DISPLAY "mecaniques traitent des piles de cartes perforees."
           DISPLAY SPACE
           DISPLAY "Que souhaitez-vous faire?"
           DISPLAY "1. Explorer le laboratoire"
           DISPLAY "2. Interagir avec le personnel"
           DISPLAY "3. Examiner l'anomalie temporelle"
           DISPLAY "4. Revenir au present"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT ACTION-CHOICE
      
           EVALUATE ACTION-CHOICE
               WHEN 1
                   DISPLAY SPACE
                   DISPLAY "Vous explorez le laboratoire et trouvez"
                   DISPLAY "plusieurs stations de perforation de cartes."
                   DISPLAY "Une en particulier semble plus sophistiquee"
                   DISPLAY "que les autres, avec un design avant-gardiste."
                   DISPLAY SPACE
                   DISPLAY "En l'examinant de plus pres, vous decouvrez"
                   DISPLAY "une serie de cartes perforees contenant ce qui"
                   DISPLAY "semble etre une version tres primitive de COBOL."
                   DISPLAY SPACE
                   DISPLAY "Vous avez decouvert un artefact: Perforateur de precision!"
                   ADD 1 TO TIME-ARTIFACTS-COLLECTED
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-1-INTERACTION
               WHEN 2
                   DISPLAY SPACE
                   DISPLAY "Vous approchez un groupe de programmeurs en"
                   DISPLAY "blouse blanche qui travaillent sur une imposante"
                   DISPLAY "armoire de calcul. Ils vous expliquent qu'ils"
                   DISPLAY "developpent un nouveau langage de programmation"
                   DISPLAY "oriente entreprise, qui deviendra plus tard COBOL."
                   DISPLAY SPACE
                   DISPLAY "L'un d'eux mentionne des 'perturbations' dans"
                   DISPLAY "leurs calculs recents, comme si des instructions"
                   DISPLAY "se modifiaient d'elles-memes..."
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-1-INTERACTION
               WHEN 3
                   IF ANOMALY-ERA(1) = 1 AND ANOMALY-FIXED(1) = "N"
                       PERFORM FIX-ERA-1-ANOMALY
                   ELSE
                       DISPLAY SPACE
                       DISPLAY "Aucune anomalie active detectee dans cette ere."
                       DISPLAY SPACE
                       DISPLAY "Appuyez sur ENTREE pour continuer..."
                       ACCEPT CONFIRMATION
                   END-IF
                   PERFORM ERA-1-INTERACTION
               WHEN 4
                   DISPLAY SPACE
                   DISPLAY "Vous activez votre dispositif de retour et"
                   DISPLAY "etes transporte vers le present."
                   PERFORM CHRONOTERMINAL-MAIN-MENU
               WHEN OTHER
                   DISPLAY "Option non reconnue."
                   PERFORM ERA-1-INTERACTION
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Correction de l'anomalie de l'ère 1
      *-----------------------------------------------------------------
       FIX-ERA-1-ANOMALY.
           DISPLAY SPACE
           DISPLAY "Vous localisez la source de l'anomalie dans"
           DISPLAY "le mecanisme de lecture des cartes perforees."
           DISPLAY "Des interferences temporelles perturbent le flux"
           DISPLAY "de donnees entre les differentes epoques."
           DISPLAY SPACE
           DISPLAY "Pour resoudre ce probleme, vous devez recalibrer"
           DISPLAY "le mecanisme en utilisant vos connaissances modernes"
           DISPLAY "de COBOL pour corriger le code primitif."
           DISPLAY SPACE
           DISPLAY "Commencer la reparation? (O/N)"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT CONFIRMATION
      
           IF CONFIRMATION = "O" OR CONFIRMATION = "o"
               DISPLAY SPACE
               DISPLAY "Vous travaillez methodiquement sur les cartes,"
               DISPLAY "reecrivant plusieurs sequences de code et"
               DISPLAY "ajustant les perforations avec precision."
               DISPLAY SPACE
               DISPLAY "Apres plusieurs heures d'effort, le systeme"
               DISPLAY "recommence a fonctionner correctement!"
               DISPLAY SPACE
               DISPLAY "Anomalie temporelle resolue!"
               MOVE "Y" TO ANOMALY-FIXED(1)
               ADD 1 TO ANOMALY-FIXED-COUNT
               ADD 10 TO TEMPORAL-STABILITY
               ADD 15 TO QUEST-PROGRESS-2
      
               IF ERA-ACCESS-STATUS(2) = "N"
                   MOVE "Y" TO ERA-ACCESS-STATUS(2)
                   DISPLAY SPACE
                   DISPLAY "*** NOUVELLE ERE DEBLOQUEE ***"
                   DISPLAY "Vous pouvez maintenant acceder a:"
                   DISPLAY ERA-NAME(2)
               END-IF
           ELSE
               DISPLAY "Vous decidez de ne pas intervenir pour l'instant."
           END-IF
      
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTREE pour continuer..."
           ACCEPT CONFIRMATION
           .
      
      *-----------------------------------------------------------------
      * Interaction avec l'ère des Mainframes
      *-----------------------------------------------------------------
       ERA-2-INTERACTION.
           DISPLAY SPACE
           DISPLAY "Vous vous trouvez dans un imposant centre de donnees"
           DISPLAY "des annees 1960. D'enormes mainframes bourdonnent"
           DISPLAY "autour de vous, traites avec reverence par des"
           DISPLAY "operateurs en blouse blanche."
           DISPLAY SPACE
           DISPLAY "Que souhaitez-vous faire?"
           DISPLAY "1. Explorer le centre de donnees"
           DISPLAY "2. Interagir avec les programmeurs"
           DISPLAY "3. Examiner l'anomalie temporelle"
           DISPLAY "4. Revenir au present"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT ACTION-CHOICE
      
           EVALUATE ACTION-CHOICE
               WHEN 1
                   DISPLAY SPACE
                   DISPLAY "Vous explorez le centre de donnees, admirant"
                   DISPLAY "les immenses armoires electroniques qui"
                   DISPLAY "contiennent moins de puissance de calcul qu'un"
                   DISPLAY "telephone moderne."
                   DISPLAY SPACE
                   DISPLAY "Dans une section securisee, vous trouvez des"
                   DISPLAY "bandes magnetiques soigneusement etiquetees"
                   DISPLAY "contenant les premieres versions commerciales"
                   DISPLAY "de COBOL."
                   DISPLAY SPACE
                   DISPLAY "Vous avez decouvert un artefact: Bande magnetique encodee!"
                   ADD 1 TO TIME-ARTIFACTS-COLLECTED
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-2-INTERACTION
               WHEN 2
                   DISPLAY SPACE
                   DISPLAY "Vous rencontrez un groupe de programmeurs COBOL"
                   DISPLAY "qui travaillent sur des applications bancaires."
                   DISPLAY "Ils sont impressionnes par votre comprehension"
                   DISPLAY "de leur langage et vous partagent des techniques"
                   DISPLAY "d'optimisation oubliees au fil du temps."
                   DISPLAY SPACE
                   DISPLAY "L'un des programmeurs seniors vous confie qu'il"
                   DISPLAY "a l'impression que certains de leurs programmes"
                   DISPLAY "se comportent comme s'ils avaient ete conçus"
                   DISPLAY "avec des connaissances du futur..."
                   DISPLAY SPACE
                   DISPLAY "Vous avez progresse dans la quete: Recuperation du code source perdu"
                   ADD 20 TO QUEST-PROGRESS-1
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-2-INTERACTION
               WHEN 3
                   DISPLAY SPACE
                   DISPLAY "Aucune anomalie active detectee dans cette ere."
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-2-INTERACTION
               WHEN 4
                   DISPLAY SPACE
                   DISPLAY "Vous activez votre dispositif de retour et"
                   DISPLAY "etes transporte vers le present."
                   PERFORM CHRONOTERMINAL-MAIN-MENU
               WHEN OTHER
                   DISPLAY "Option non reconnue."
                   PERFORM ERA-2-INTERACTION
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Interaction avec l'ère Microinformatique
      *-----------------------------------------------------------------
       ERA-3-INTERACTION.
           DISPLAY SPACE
           DISPLAY "Vous vous trouvez dans un bureau des annees 1980,"
           DISPLAY "entoure d'ordinateurs personnels aux cotes de plus"
           DISPLAY "gros systemes departementaux. Des disquettes sont"
           DISPLAY "empilees sur les bureaux."
           DISPLAY SPACE
           DISPLAY "Que souhaitez-vous faire?"
           DISPLAY "1. Explorer les ordinateurs"
           DISPLAY "2. Parler aux informaticiens"
           DISPLAY "3. Examiner l'anomalie temporelle"
           DISPLAY "4. Revenir au present"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT ACTION-CHOICE
      
           EVALUATE ACTION-CHOICE
               WHEN 1
                   DISPLAY SPACE
                   DISPLAY "Vous examinez les differents modeles d'ordinateurs,"
                   DISPLAY "des IBM PC aux premiers Apple. Sur l'un d'eux,"
                   DISPLAY "vous remarquez un programme de terminal qui"
                   DISPLAY "permet de se connecter aux mainframes COBOL."
                   DISPLAY SPACE
                   DISPLAY "A cote, vous trouvez une disquette etiquetee"
                   DISPLAY "'SYSTEM.BOOT' avec des marquages non standard."
                   DISPLAY SPACE
                   DISPLAY "Vous avez decouvert un artefact: Disquette de demarrage!"
                   ADD 1 TO TIME-ARTIFACTS-COLLECTED
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-3-INTERACTION
               WHEN 2
                   DISPLAY SPACE
                   DISPLAY "Vous discutez avec des informaticiens qui"
                   DISPLAY "travaillent a connecter les nouveaux PC aux"
                   DISPLAY "systemes mainframe existants. Ils sont confrontes"
                   DISPLAY "a des defis d'integration et de compatibilite."
                   DISPLAY SPACE
                   DISPLAY "Un developpeur senior vous montre comment ils"
                   DISPLAY "adaptent le code COBOL pour fonctionner avec"
                   DISPLAY "les nouvelles interfaces utilisateur graphiques."
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-3-INTERACTION
               WHEN 3
                   IF ANOMALY-ERA(2) = 3 AND ANOMALY-FIXED(2) = "N"
                       PERFORM FIX-ERA-3-ANOMALY
                   ELSE
                       DISPLAY SPACE
                       DISPLAY "Aucune anomalie active detectee dans cette ere."
                       DISPLAY SPACE
                       DISPLAY "Appuyez sur ENTREE pour continuer..."
                       ACCEPT CONFIRMATION
                   END-IF
                   PERFORM ERA-3-INTERACTION
               WHEN 4
                   DISPLAY SPACE
                   DISPLAY "Vous activez votre dispositif de retour et"
                   DISPLAY "etes transporte vers le present."
                   PERFORM CHRONOTERMINAL-MAIN-MENU
               WHEN OTHER
                   DISPLAY "Option non reconnue."
                   PERFORM ERA-3-INTERACTION
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Correction de l'anomalie de l'ère 3
      *-----------------------------------------------------------------
       FIX-ERA-3-ANOMALY.
           DISPLAY SPACE
           DISPLAY "Vous detectez une corruption dans l'architecture"
           DISPLAY "de la memoire virtuelle des systemes. Cette anomalie"
           DISPLAY "semble causer des interferences entre les epoques."
           DISPLAY SPACE
           DISPLAY "Pour resoudre ce probleme, vous devez recompiler"
           DISPLAY "les pilotes memoire avec des corrections qui"
           DISPLAY "stabiliseront le flux temporel."
           DISPLAY SPACE
           DISPLAY "Commencer la reparation? (O/N)"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT CONFIRMATION
      
           IF CONFIRMATION = "O" OR CONFIRMATION = "o"
               DISPLAY SPACE
               DISPLAY "Vous travaillez sur le code assembleur des"
               DISPLAY "gestionnaires de memoire, identifiant et corrigeant"
               DISPLAY "les segments corrompu par les fluctuations temporelles."
               DISPLAY SPACE
               DISPLAY "Apres un travail minutieux, vous parvenez a"
               DISPLAY "stabiliser l'architecture memoire!"
               DISPLAY SPACE
               DISPLAY "Anomalie temporelle resolue!"
               MOVE "Y" TO ANOMALY-FIXED(2)
               ADD 1 TO ANOMALY-FIXED-COUNT
               ADD 15 TO TEMPORAL-STABILITY
               ADD 25 TO QUEST-PROGRESS-2
      
               IF ERA-ACCESS-STATUS(4) = "N"
                   MOVE "Y" TO ERA-ACCESS-STATUS(4)
                   DISPLAY SPACE
                   DISPLAY "*** NOUVELLE ERE DEBLOQUEE ***"
                   DISPLAY "Vous pouvez maintenant acceder a:"
                   DISPLAY ERA-NAME(4)
               END-IF
           ELSE
               DISPLAY "Vous decidez de ne pas intervenir pour l'instant."
           END-IF
      
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTREE pour continuer..."
           ACCEPT CONFIRMATION
           .
      
      *-----------------------------------------------------------------
      * Interaction avec l'ère Internet
      *-----------------------------------------------------------------
       ERA-4-INTERACTION.
           DISPLAY SPACE
           DISPLAY "Vous vous trouvez dans un centre technique des"
           DISPLAY "annees 1990, au debut de l'ere Internet. Des serveurs"
           DISPLAY "web cotoient d'anciens mainframes, tandis que des"
           DISPLAY "developpeurs travaillent a les interconnecter."
           DISPLAY SPACE
           DISPLAY "Que souhaitez-vous faire?"
           DISPLAY "1. Explorer l'infrastructure reseau"
           DISPLAY "2. Parler aux ingenieurs web"
           DISPLAY "3. Examiner l'anomalie temporelle"
           DISPLAY "4. Revenir au present"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT ACTION-CHOICE
      
           EVALUATE ACTION-CHOICE
               WHEN 1
                   DISPLAY SPACE
                   DISPLAY "Vous explorez la salle des serveurs ou cohabitent"
                   DISPLAY "des technologies de differentes epoques. Des"
                   DISPLAY "passerelles personnalisees permettent aux mainframes"
                   DISPLAY "COBOL de communiquer avec les nouveaux serveurs web."
                   DISPLAY SPACE
                   DISPLAY "Dans un rack isole, vous trouvez un prototype"
                   DISPLAY "de routeur avec des specifications inhabituelles."
                   DISPLAY SPACE
                   DISPLAY "Vous avez decouvert un artefact: Routeur prototype!"
                   ADD 1 TO TIME-ARTIFACTS-COLLECTED
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-4-INTERACTION
               WHEN 2
                   DISPLAY SPACE
                   DISPLAY "Vous discutez avec une equipe qui developpe"
                   DISPLAY "des interfaces pour exposer les fonctionnalites"
                   DISPLAY "COBOL via le web emergent. Ils vous expliquent"
                   DISPLAY "leurs techniques pour faire cohabiter ces mondes."
                   DISPLAY SPACE
                   DISPLAY "Un developpeur vous montre un fragment de code"
                   DISPLAY "COBOL particulierement elegant qui semble avoir"
                   DISPLAY "ete ecrit avec une comprehension profonde des"
                   DISPLAY "technologies futures."
                   DISPLAY SPACE
                   DISPLAY "Vous avez progresse dans la quete: Recuperation du code source perdu"
                   ADD 30 TO QUEST-PROGRESS-1
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-4-INTERACTION
               WHEN 3
                   IF ANOMALY-ERA(3) = 4 AND ANOMALY-FIXED(3) = "N"
                       PERFORM FIX-ERA-4-ANOMALY
                   ELSE
                       DISPLAY SPACE
                       DISPLAY "Aucune anomalie active detectee dans cette ere."
                       DISPLAY SPACE
                       DISPLAY "Appuyez sur ENTREE pour continuer..."
                       ACCEPT CONFIRMATION
                   END-IF
                   PERFORM ERA-4-INTERACTION
               WHEN 4
                   DISPLAY SPACE
                   DISPLAY "Vous activez votre dispositif de retour et"
                   DISPLAY "etes transporte vers le present."
                   PERFORM CHRONOTERMINAL-MAIN-MENU
               WHEN OTHER
                   DISPLAY "Option non reconnue."
                   PERFORM ERA-4-INTERACTION
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Correction de l'anomalie de l'ère 4
      *-----------------------------------------------------------------
       FIX-ERA-4-ANOMALY.
           DISPLAY SPACE
           DISPLAY "Vous identifiez des interferences temporelles dans"
           DISPLAY "les protocoles reseau. Les paquets de donnees semblent"
           DISPLAY "se melanger entre les differentes epoques, creant"
           DISPLAY "des perturbations a travers le continuum."
           DISPLAY SPACE
           DISPLAY "Pour resoudre ce probleme, vous devez implementer"
           DISPLAY "un filtre temporel dans les routeurs pour isoler"
           DISPLAY "le trafic de chaque epoque."
           DISPLAY SPACE
           DISPLAY "Commencer la reparation? (O/N)"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT CONFIRMATION
      
           IF CONFIRMATION = "O" OR CONFIRMATION = "o"
               DISPLAY SPACE
               DISPLAY "Vous developpez un algorithme sophistique de"
               DISPLAY "filtrage qui identifie et separe les paquets"
               DISPLAY "en fonction de leur signature temporelle."
               DISPLAY SPACE
               DISPLAY "Une fois deploye, le filtre commence immediatement"
               DISPLAY "a stabiliser les communications entre les epoques!"
               DISPLAY SPACE
               DISPLAY "Anomalie temporelle resolue!"
               MOVE "Y" TO ANOMALY-FIXED(3)
               ADD 1 TO ANOMALY-FIXED-COUNT
               ADD 20 TO TEMPORAL-STABILITY
               ADD 35 TO QUEST-PROGRESS-2
      
               IF ERA-ACCESS-STATUS(5) = "N"
                   MOVE "Y" TO ERA-ACCESS-STATUS(5)
                   DISPLAY SPACE
                   DISPLAY "*** NOUVELLE ERE DEBLOQUEE ***"
                   DISPLAY "Vous pouvez maintenant acceder a:"
                   DISPLAY ERA-NAME(5)
               END-IF
           ELSE
               DISPLAY "Vous decidez de ne pas intervenir pour l'instant."
           END-IF
      
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTREE pour continuer..."
           ACCEPT CONFIRMATION
           .
      
      *-----------------------------------------------------------------
      * Interaction avec l'ère Cloud
      *-----------------------------------------------------------------
       ERA-5-INTERACTION.
           DISPLAY SPACE
           DISPLAY "Vous vous trouvez dans un centre de donnees moderne,"
           DISPLAY "rempli de racks de serveurs. Les mainframes physiques"
           DISPLAY "d'autrefois ont largement ete virtualises, mais les"
           DISPLAY "systemes COBOL fonctionnent toujours."
           DISPLAY SPACE
           DISPLAY "Que souhaitez-vous faire?"
           DISPLAY "1. Explorer l'infrastructure cloud"
           DISPLAY "2. Parler aux architectes systeme"
           DISPLAY "3. Rechercher des anomalies temporelles"
           DISPLAY "4. Revenir au present"
           DISPLAY "> " WITH NO ADVANCING
      
           ACCEPT ACTION-CHOICE
      
           EVALUATE ACTION-CHOICE
               WHEN 1
                   DISPLAY SPACE
                   DISPLAY "Vous examinez l'architecture cloud moderne ou"
                   DISPLAY "des conteneurs virtuels hebergent des systemes"
                   DISPLAY "COBOL dans un environnement hautement disponible."
                   DISPLAY SPACE
                   DISPLAY "Dans une section securisee, vous trouvez un prototype"
                   DISPLAY "de systeme d'authentification qui utilise des"
                   DISPLAY "principes quantiques pour verifier l'identite."
                   DISPLAY SPACE
                   DISPLAY "Vous avez decouvert un artefact: Jeton d'authentification quantique!"
                   ADD 1 TO TIME-ARTIFACTS-COLLECTED
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-5-INTERACTION
               WHEN 2
                   DISPLAY SPACE
                   DISPLAY "Vous discutez avec des architectes cloud qui"
                   DISPLAY "continuent a maintenir et faire evoluer les"
                   DISPLAY "systemes COBOL critiques. Ils vous expliquent"
                   DISPLAY "comment ils ont modernise ces applications"
                   DISPLAY "sans perdre leur fiabilite legendaire."
                   DISPLAY SPACE
                   DISPLAY "Ils vous montrent un module COBOL particulier"
                   DISPLAY "qui semble contenir des fragments de code datant"
                   DISPLAY "de toutes les epoques precedentes, comme une"
                   DISPLAY "capsule temporelle de l'evolution du langage."
                   DISPLAY SPACE
                   DISPLAY "Vous avez progresse significativement dans la quete:"
                   DISPLAY "Recuperation du code source perdu"
                   ADD 50 TO QUEST-PROGRESS-1
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-5-INTERACTION
               WHEN 3
                   DISPLAY SPACE
                   DISPLAY "Vous scannez cette epoque a la recherche"
                   DISPLAY "d'anomalies temporelles..."
                   DISPLAY SPACE
                   DISPLAY "Aucune anomalie detectee dans cette ere."
                   DISPLAY "Cependant, vous remarquez que cette epoque semble"
                   DISPLAY "etre le point de convergence des fluctuations"
                   DISPLAY "temporelles des autres epoques."
                   DISPLAY SPACE
                   DISPLAY "Appuyez sur ENTREE pour continuer..."
                   ACCEPT CONFIRMATION
                   PERFORM ERA-5-INTERACTION
               WHEN 4
                   DISPLAY SPACE
                   DISPLAY "Vous activez votre dispositif de retour et"
                   DISPLAY "etes transporte vers le present."
                   PERFORM CHRONOTERMINAL-MAIN-MENU
               WHEN OTHER
                   DISPLAY "Option non reconnue."
                   PERFORM ERA-5-INTERACTION
           END-EVALUATE
           .
      
      *-----------------------------------------------------------------
      * Examen des anomalies temporelles
      *-----------------------------------------------------------------
       EXAMINE-ANOMALIES.
           DISPLAY SPACE
           DISPLAY "=== ANOMALIES TEMPORELLES DETECTEES ==="
           DISPLAY SPACE
           DISPLAY "Progression de resolution: " 
               ANOMALY-FIXED-COUNT "/" ANOMALY-COUNT
           DISPLAY SPACE
      
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ANOMALY-COUNT
               DISPLAY "Anomalie " I ": " ANOMALY-DESC(I)
               DISPLAY "   Localisation: Ere " ANOMALY-ERA(I) 
                   " - " ERA-NAME(ANOMALY-ERA(I))
               IF ANOMALY-FIXED(I) = "Y"
                   DISPLAY "   Statut: RESOLUE"
               ELSE
                   DISPLAY "   Statut: ACTIVE"
               END-IF
               DISPLAY SPACE
           END-PERFORM
      
           DISPLAY "Appuyez sur ENTREE pour revenir au menu..."
           ACCEPT CONFIRMATION
           PERFORM CHRONOTERMINAL-MAIN-MENU
           .
      
      *-----------------------------------------------------------------
      * Consultation des artefacts temporels
      *-----------------------------------------------------------------
       VIEW-ARTIFACTS.
           DISPLAY SPACE
           DISPLAY "=== ARTEFACTS TEMPORELS COLLECTES ==="
           DISPLAY SPACE
           DISPLAY "Total: " TIME-ARTIFACTS-COLLECTED "/10"
           DISPLAY SPACE
      
           IF TIME-ARTIFACTS-COLLECTED >= 1
               DISPLAY "- " ARTIFACT-1-1
               DISPLAY "  Origine: Ere des Cartes Perforees"
               DISPLAY SPACE
           END-IF
      
           IF TIME-ARTIFACTS-COLLECTED >= 2
               DISPLAY "- " ARTIFACT-1-2
               DISPLAY "  Origine: Ere des Cartes Perforees"
               DISPLAY SPACE
           END-IF
      
           IF TIME-ARTIFACTS-COLLECTED >= 3
               DISPLAY "- " ARTIFACT-2-1
               DISPLAY "  Origine: Ere des Mainframes"
               DISPLAY SPACE
           END-IF
      
           IF TIME-ARTIFACTS-COLLECTED >= 4
               DISPLAY "- " ARTIFACT-2-2
               DISPLAY "  Origine: Ere des Mainframes"
               DISPLAY SPACE
           END-IF
      
           IF TIME-ARTIFACTS-COLLECTED >= 5
               DISPLAY "- " ARTIFACT-3-1
               DISPLAY "  Origine: Ere Microinformatique"
               DISPLAY SPACE
           END-IF
      
           DISPLAY "Appuyez sur ENTREE pour revenir au menu..."
           ACCEPT CONFIRMATION
           PERFORM CHRONOTERMINAL-MAIN-MENU
           .
      
      *-----------------------------------------------------------------
      * Vérification du statut des quêtes temporelles
      *-----------------------------------------------------------------
       CHECK-TEMPORAL-QUESTS.
           DISPLAY SPACE
           DISPLAY "=== QUETES TEMPORELLES ==="
           DISPLAY SPACE
      
           DISPLAY "1. " MAIN-QUEST-1
           COMPUTE QUEST-COMPLETION = QUEST-PROGRESS-1 / 100 * 100
           DISPLAY "   Progression: " QUEST-PROGRESS-1 "% complete"
           DISPLAY SPACE
           DISPLAY "   Description: Retrouvez les fragments du code"
           DISPLAY "   source original de COBOL disperses a travers"
           DISPLAY "   les epoques pour comprendre l'origine de"
           DISPLAY "   MAINFRAME-TERRA."
           DISPLAY SPACE
      
           DISPLAY "2. " MAIN-QUEST-2
           COMPUTE QUEST-COMPLETION = QUEST-PROGRESS-2 / 100 * 100
           DISPLAY "   Progression: " QUEST-PROGRESS-2 "% complete"
           DISPLAY SPACE
           DISPLAY "   Description: Identifiez et corrigez les anomalies"
           DISPLAY "   temporelles qui perturbent le flux entre les"
           DISPLAY "   epoques pour stabiliser le continuum."
           DISPLAY SPACE
      
           IF QUEST-PROGRESS-1 >= 100 AND QUEST-PROGRESS-2 >= 100
               DISPLAY "*** FELICITATIONS ***"
               DISPLAY "Vous avez complete toutes les quetes temporelles!"
               DISPLAY "Grace a vos actions, MAINFRAME-TERRA est sauvee"
               DISPLAY "et le flux temporel est stabilise."
               DISPLAY SPACE
               DISPLAY "Le code source originel de COBOL a ete preserve"
               DISPLAY "a travers les ages, assurant l'existence meme"
               DISPLAY "de notre monde."
           END-IF
      
           DISPLAY SPACE
           DISPLAY "Appuyez sur ENTREE pour revenir au menu..."
           ACCEPT CONFIRMATION
           PERFORM CHRONOTERMINAL-MAIN-MENU
           .
      
       END PROGRAM TERMINAL-TIME-TRAVEL.