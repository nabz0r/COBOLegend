      *****************************************************************
      * EASTER_EGGS.CBL - Surprises cachées dans COBOLegend
      *
      * Ce fichier contient des éléments secrets qui peuvent être
      * découverts par les joueurs curieux qui parcourent le code source.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EASTER-EGGS.
       AUTHOR. NABZ0R.
       DATE-WRITTEN. 2025-03-04.
       SECURITY. TOP-SECRET.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IMAGINATION-MACHINE.
       OBJECT-COMPUTER. YOUR-BRAIN.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01  SECRET-CODES.
           05  KONAMI-CODE          PIC X(10) VALUE "UUDDLRLRBA".
           05  INFINITE-GOLD-CODE   PIC X(13) VALUE "GOSHDARNBUGS".
           05  GOD-MODE-CODE        PIC X(9)  VALUE "COBOLIZER".
           05  UNLOCK-ALL-CODE      PIC X(12) VALUE "MAINFRAMEKEY".
      
       01  SECRET-DIALOGUE.
           05  HIDDEN-MESSAGE-1     PIC X(50) VALUE
               "Dans l'ombre des mainframes, les anciens attendent...".
           05  HIDDEN-MESSAGE-2     PIC X(50) VALUE
               "Cherchez le terminal abandonné au nord-est de la cité.".
           05  HIDDEN-MESSAGE-3     PIC X(50) VALUE
               "Le véritable pouvoir réside dans le COMPUTATIONAL-5.".
      
       01  SECRET-ITEMS.
           05  ITEM-1.
               10  ITEM-NAME        PIC X(30) VALUE
                   "Disquette 8 pouces de l'Ancien Monde".
               10  ITEM-DESC        PIC X(100) VALUE
                   "Un artefact rare contenant des secrets oubliés. "
                   "Peut être utilisé dans le Terminal des Archives.".
           05  ITEM-2.
               10  ITEM-NAME        PIC X(30) VALUE
                   "Badge d'accès du Développeur".
               10  ITEM-DESC        PIC X(100) VALUE
                   "Un badge permettant d'accéder aux zones de "
                   "développement des mainframes. Niveau TOP SECRET.".
           05  ITEM-3.
               10  ITEM-NAME        PIC X(30) VALUE
                   "Manuel COBOL 1969 Original".
               10  ITEM-DESC        PIC X(100) VALUE
                   "Le manuscrit légendaire contenant des incantations "
                   "COBOL perdues et des techniques oubliées.".
      
       01  SECRET-LOCATION.
           05  LOCATION-NAME        PIC X(30) VALUE
               "Chambre des Échos Numériques".
           05  LOCATION-DESC        PIC X(255) VALUE
               "Une pièce cachée où résonnent les échos des premiers "
               "programmes COBOL jamais exécutés. Les murs sont couverts "
               "de symboles et d'inscriptions anciennes. Au centre, un "
               "terminal lumineux affiche un prompt clignotant. Sur une "
               "plaque près de l'entrée, on peut lire: 'Ceux qui ont "
               "programmé avant nous ont ouvert la voie'.".
      
       01  ASCII-ART.
           05  HIDDEN-SHIP          PIC X(20) VALUE
               "    /\     ".
           05  HIDDEN-SHIP-2        PIC X(20) VALUE
               "   /  \    ".
           05  HIDDEN-SHIP-3        PIC X(20) VALUE
               "  /____\   ".
           05  HIDDEN-SHIP-4        PIC X(20) VALUE
               "  |    |   ".
           05  HIDDEN-SHIP-5        PIC X(20) VALUE
               " /      \  ".
           05  HIDDEN-SHIP-6        PIC X(20) VALUE
               "/__/\__\  ".
      
       01  SECRET-ENDING.
           05  ENDING-TEXT          PIC X(255) VALUE
               "Alors que vous insérez la Clé d'accès mainframe dans le "
               "terminal central, l'écran s'éclaire d'une lueur bleuâtre. "
               "Les mots 'MAINFRAME-TERRA OS 2.0 INITIALIZING' "
               "apparaissent. Vous venez de découvrir que le monde "
               "entier n'est qu'une simulation COBOL lancée il y a des "
               "décennies et toujours en cours d'exécution. Vous avez "
               "maintenant accès aux paramètres du monde...".
      
       01  DEVELOPER-CREDITS.
           05  CREDIT-LINE-1        PIC X(50) VALUE
               "COBOLegend a été créé avec passion par Claude".
           05  CREDIT-LINE-2        PIC X(50) VALUE
               "Remerciements spéciaux à Grace Hopper et aux".
           05  CREDIT-LINE-3        PIC X(50) VALUE
               "pionniers qui ont développé COBOL.".
           05  CREDIT-LINE-4        PIC X(50) VALUE
               "Dédié à tous les mainframes qui tournent encore".
           05  CREDIT-LINE-5        PIC X(50) VALUE
               "fidèlement après toutes ces années.".
      
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      
           DISPLAY "Si vous lisez ceci, vous avez découvert un"
               " des secrets de COBOLegend.".
           DISPLAY "Continuez à explorer le code et le jeu!"
               " D'autres surprises vous attendent...".
      
      *-----------------------------------------------------------------
      * Combinaison secrète: Entrez 'PERFORM SECRET-UNLOCK' dans le
      * terminal abandonné pour débloquer un objet spécial.
      *-----------------------------------------------------------------
       SECRET-UNLOCK.
           DISPLAY SPACE.
           DISPLAY "**** VOUS AVEZ DÉCOUVERT UN SECRET ****".
           DISPLAY "Vous obtenez: " ITEM-NAME OF ITEM-3.
           DISPLAY ITEM-DESC OF ITEM-3.
           DISPLAY SPACE.
           DISPLAY "Ajoutez cet objet à votre inventaire en modifiant"
               " manuellement le code du jeu!".
           DISPLAY SPACE.
      
      *-----------------------------------------------------------------
      * Pour débloquer la fin alternative, recherchez le terminal dans
      * la "Chambre des Échos Numériques" et entrez le KONAMI-CODE.
      *-----------------------------------------------------------------
       SHOW-ALTERNATE-ENDING.
           DISPLAY ENDING-TEXT.
           DISPLAY SPACE.
           DISPLAY ASCII-ART.
           DISPLAY HIDDEN-SHIP.
           DISPLAY HIDDEN-SHIP-2.
           DISPLAY HIDDEN-SHIP-3.
           DISPLAY HIDDEN-SHIP-4.
           DISPLAY HIDDEN-SHIP-5.
           DISPLAY HIDDEN-SHIP-6.
           DISPLAY SPACE.
           DISPLAY DEVELOPER-CREDITS.
           DISPLAY CREDIT-LINE-1.
           DISPLAY CREDIT-LINE-2.
           DISPLAY CREDIT-LINE-3.
           DISPLAY CREDIT-LINE-4.
           DISPLAY CREDIT-LINE-5.
      
           STOP RUN.
      
       END PROGRAM EASTER-EGGS.
