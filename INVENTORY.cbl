      *****************************************************************
      * INVENTORY.CBL - Système d'inventaire de COBOLegend
      *
      * Ce module gère tous les aspects liés à l'inventaire du joueur,
      * incluant les objets, l'équipement, et l'utilisation d'objets.
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-SYSTEM.
       AUTHOR. CLAUDE.
       DATE-WRITTEN. 2025-03-04.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *-----------------------------------------------------------------
      * Constantes d'inventaire
      *-----------------------------------------------------------------
       01 MAX-INVENTORY-SIZE         PIC 9(2)   VALUE 20.
       01 MAX-EQUIPMENT-SLOTS        PIC 9(1)   VALUE 5.
      
      *-----------------------------------------------------------------
      * Structure des objets et de l'inventaire
      *-----------------------------------------------------------------
       01 PLAYER-INVENTORY.
          05 INVENTORY-COUNT         PIC 9(2)   VALUE 0.
          05 INVENTORY-ITEMS OCCURS 20 TIMES INDEXED BY INV-IDX.
             10 ITEM-ID              PIC 9(3).
             10 ITEM-NAME            PIC X(20).
             10 ITEM-DESC            PIC X(100).
             10 ITEM-TYPE            PIC X(1).
                88 ITEM-WEAPON       VALUE 'W'.
                88 ITEM-ARMOR        VALUE 'A'.
                88 ITEM-CONSUMABLE   VALUE 'C'.
                88 ITEM-QUEST        VALUE 'Q'.
                88 ITEM-MISC         VALUE 'M'.
             10 ITEM-VALUE           PIC 9(5).
             10 ITEM-QUANTITY        PIC 9(2).
             10 ITEM-EQUIPPED        PIC X(1)   VALUE 'N'.
                88 IS-EQUIPPED       VALUE 'Y'.
             10 ITEM-STATS.
                15 ITEM-ATTACK       PIC S9(3).
                15 ITEM-DEFENSE      PIC S9(3).
                15 ITEM-HEAL         PIC 9(3).
                15 ITEM-MANA         PIC 9(3).
      
       01 PLAYER-GOLD                PIC 9(6)   VALUE 100.
      
      *-----------------------------------------------------------------
      * Slots d'équipement
      *-----------------------------------------------------------------
       01 PLAYER-EQUIPMENT.
          05 EQUIPMENT-SLOTS OCCURS 5 TIMES.
             10 EQUIP-TYPE           PIC X(10).
             10 EQUIP-ITEM-ID        PIC 9(3)   VALUE 0.
             10 EQUIP-BONUS          PIC S9(3)  VALUE 0.
      
       PROCEDURE DIVISION.
      
      *-----------------------------------------------------------------
      * Initialisation de l'inventaire et de l'équipement
      *-----------------------------------------------------------------
       INITIALIZE-INVENTORY.
           PERFORM SETUP-EQUIPMENT-SLOTS
           PERFORM ADD-STARTING-ITEMS
           .
      
       SETUP-EQUIPMENT-SLOTS.
           MOVE "Tête"       TO EQUIP-TYPE(1)
           MOVE "Torse"      TO EQUIP-TYPE(2)
           MOVE "Arme"       TO EQUIP-TYPE(3)
           MOVE "Bouclier"   TO EQUIP-TYPE(4)
           MOVE "Accessoire" TO EQUIP-TYPE(5)
           .
      
       ADD-STARTING-ITEMS.
           PERFORM ADD-ITEM-TO-INVENTORY
               USING 1 "Épée rouillée" 
                     "Une vieille épée rouillée, mais toujours "
                     "fonctionnelle." "W" 10 1
               GIVING WAS-ADDED
           PERFORM ADD-ITEM-TO-INVENTORY
               USING 2 "Bouclier en bois" 
                     "Un simple bouclier en bois qui offre une "
                     "protection minimale." "A" 5 1
               GIVING WAS-ADDED
           PERFORM ADD-ITEM-TO-INVENTORY
               USING 3 "Potion de soin" 
                     "Une potion qui restaure 15 points de vie." 
                     "C" 20 3
               GIVING WAS-ADDED
      
           MOVE 1 TO ITEM-ATTACK(1)
           MOVE 0 TO ITEM-DEFENSE(1)
           MOVE 0 TO ITEM-HEAL(1)
           MOVE 0 TO ITEM-MANA(1)
      
           MOVE 0 TO ITEM-ATTACK(2)
           MOVE 1 TO ITEM-DEFENSE(2)
           MOVE 0 TO ITEM-HEAL(2)
           MOVE 0 TO ITEM-MANA(2)
      
           MOVE 0 TO ITEM-ATTACK(3)
           MOVE 0 TO ITEM-DEFENSE(3)
           MOVE 15 TO ITEM-HEAL(3)
           MOVE 0 TO ITEM-MANA(3)
      
           PERFORM EQUIP-ITEM USING 1
           PERFORM EQUIP-ITEM USING 2
           .
      
      *-----------------------------------------------------------------
      * Ajout d'un objet à l'inventaire
      *-----------------------------------------------------------------
       ADD-ITEM-TO-INVENTORY.
           *> Paramètres: ITEM-ID-TO-ADD ITEM-NAME-TO-ADD ITEM-DESC-TO-ADD
           *>            ITEM-TYPE-TO-ADD ITEM-VALUE-TO-ADD 
           *>            ITEM-QUANTITY-TO-ADD
           *> Retourne: WAS-ADDED (Y/N)
      
           *> Vérifier si l'objet est déjà présent (pour les empilables)
           PERFORM VARYING INV-IDX FROM 1 BY 1 
                   UNTIL INV-IDX > INVENTORY-COUNT
               IF ITEM-ID(INV-IDX) = ITEM-ID-TO-ADD AND
                  (ITEM-TYPE(INV-IDX) = "C" OR ITEM-TYPE(INV-IDX) = "M")
                   ADD ITEM-QUANTITY-TO-ADD TO ITEM-QUANTITY(INV-IDX)
                   MOVE "Y" TO WAS-ADDED
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
      
           *> Vérifier si l'inventaire est plein
           IF INVENTORY-COUNT >= MAX-INVENTORY-SIZE
               MOVE "N" TO WAS-ADDED
               EXIT PARAGRAPH
           END-IF
      
           *> Ajouter le nouvel objet
           ADD 1 TO INVENTORY-COUNT
           MOVE ITEM-ID-TO-ADD TO ITEM-ID(INVENTORY-COUNT)
           MOVE ITEM-NAME-TO-ADD TO ITEM-NAME(INVENTORY-COUNT)
           MOVE ITEM-DESC-TO-ADD TO ITEM-DESC(INVENTORY-COUNT)
           MOVE ITEM-TYPE-TO-ADD TO ITEM-TYPE(INVENTORY-COUNT)
           MOVE ITEM-VALUE-TO-ADD TO ITEM-VALUE(INVENTORY-COUNT)
           MOVE ITEM-QUANTITY-TO-ADD TO ITEM-QUANTITY(INVENTORY-COUNT)
           MOVE "N" TO ITEM-EQUIPPED(INVENTORY-COUNT)
           MOVE 0 TO ITEM-ATTACK(INVENTORY-COUNT)
           MOVE 0 TO ITEM-DEFENSE(INVENTORY-COUNT)
           MOVE 0 TO ITEM-HEAL(INVENTORY-COUNT)
           MOVE 0 TO ITEM-MANA(INVENTORY-COUNT)
      
           MOVE "Y" TO WAS-ADDED
           DISPLAY "Objet ajouté: " ITEM-NAME-TO-ADD
           .
      
      *-----------------------------------------------------------------
      * Équipement d'un objet
      *-----------------------------------------------------------------
       EQUIP-ITEM.
           *> Paramètre: ITEM-IDX-TO-EQUIP
      
           *> Vérifier si l'objet est équipable
           IF ITEM-TYPE(ITEM-IDX-TO-EQUIP) NOT = "W" AND
              ITEM-TYPE(ITEM-IDX-TO-EQUIP) NOT = "A"
               DISPLAY "Cet objet ne peut pas être équipé."
               EXIT PARAGRAPH
           END-IF
      
           *> Déterminer le slot d'équipement approprié
           IF ITEM-TYPE(ITEM-IDX-TO-EQUIP) = "W"
               MOVE 3 TO EQUIPMENT-SLOT
           ELSE
               IF ITEM-NAME(ITEM-IDX-TO-EQUIP) = "Bouclier en bois" OR
                  ITEM-NAME(ITEM-IDX-TO-EQUIP) = "Bouclier de fer"
                   MOVE 4 TO EQUIPMENT-SLOT
               ELSE
                   MOVE 2 TO EQUIPMENT-SLOT
               END-IF
           END-IF
      
           *> Déséquiper l'objet actuel si présent
           IF EQUIP-ITEM-ID(EQUIPMENT-SLOT) > 0
               PERFORM VARYING INV-IDX FROM 1 BY 1 
                       UNTIL INV-IDX > INVENTORY-COUNT
                   IF ITEM-ID(INV-IDX) = EQUIP-ITEM-ID(EQUIPMENT-SLOT)
                       MOVE "N" TO ITEM-EQUIPPED(INV-IDX)
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF
      
           *> Équiper le nouvel objet
           MOVE ITEM-ID(ITEM-IDX-TO-EQUIP) TO 
                EQUIP-ITEM-ID(EQUIPMENT-SLOT)
           MOVE "Y" TO ITEM-EQUIPPED(ITEM-IDX-TO-EQUIP)
      
           *> Mettre à jour les bonus
           IF ITEM-TYPE(ITEM-IDX-TO-EQUIP) = "W"
               MOVE ITEM-ATTACK(ITEM-IDX-TO-EQUIP) TO 
                    EQUIP-BONUS(EQUIPMENT-SLOT)
               COMPUTE WEAPON-BONUS = SUM OF EQUIP-BONUS(3)
           ELSE
               MOVE ITEM-DEFENSE(ITEM-IDX-TO-EQUIP) TO 
                    EQUIP-BONUS(EQUIPMENT-SLOT)
               COMPUTE ARMOR-BONUS = 
                   SUM OF EQUIP-BONUS(1 2 4 5)
           END-IF
      
           DISPLAY "Vous équipez: " ITEM-NAME(ITEM-IDX-TO-EQUIP)
           .
      
      *-----------------------------------------------------------------
      * Utilisation d'un objet consommable
      *-----------------------------------------------------------------
       USE-CONSUMABLE.
           *> Paramètre: ITEM-IDX-TO-USE
      
           *> Vérifier si c'est un consommable
           IF ITEM-TYPE(ITEM-IDX-TO-USE) NOT = "C"
               DISPLAY "Cet objet ne peut pas être utilisé ainsi."
               EXIT PARAGRAPH
           END-IF
      
           *> Appliquer les effets
           IF ITEM-HEAL(ITEM-IDX-TO-USE) > 0
               ADD ITEM-HEAL(ITEM-IDX-TO-USE) TO CHAR-HEALTH-CURRENT
               IF CHAR-HEALTH-CURRENT > CHAR-HEALTH-MAX
                   MOVE CHAR-HEALTH-MAX TO CHAR-HEALTH-CURRENT
               END-IF
               DISPLAY "Vous récupérez " ITEM-HEAL(ITEM-IDX-TO-USE) 
                   " points de vie."
           END-IF
      
           IF ITEM-MANA(ITEM-IDX-TO-USE) > 0
               ADD ITEM-MANA(ITEM-IDX-TO-USE) TO CHAR-MANA-CURRENT
               IF CHAR-MANA-CURRENT > CHAR-MANA-MAX
                   MOVE CHAR-MANA-MAX TO CHAR-MANA-CURRENT
               END-IF
               DISPLAY "Vous récupérez " ITEM-MANA(ITEM-IDX-TO-USE) 
                   " points de mana."
           END-IF
      
           *> Réduire la quantité
           SUBTRACT 1 FROM ITEM-QUANTITY(ITEM-IDX-TO-USE)
           IF ITEM-QUANTITY(ITEM-IDX-TO-USE) <= 0
               PERFORM REMOVE-ITEM-FROM-INVENTORY
           END-IF
           .
      
      *-----------------------------------------------------------------
      * Suppression d'un objet de l'inventaire
      *-----------------------------------------------------------------
       REMOVE-ITEM-FROM-INVENTORY.
           *> Paramètre: ITEM-IDX-TO-REMOVE
      
           *> Décaler tous les éléments suivants
           PERFORM VARYING MOVE-IDX FROM ITEM-IDX-TO-REMOVE BY 1
                   UNTIL MOVE-IDX >= INVENTORY-COUNT
               ADD 1 TO MOVE-IDX GIVING TARGET-IDX
               MOVE ITEM-ID(TARGET-IDX) TO ITEM-ID(MOVE-IDX)
               MOVE ITEM-NAME(TARGET-IDX) TO ITEM-NAME(MOVE-IDX)
               MOVE ITEM-DESC(TARGET-IDX) TO ITEM-DESC(MOVE-IDX)
               MOVE ITEM-TYPE(TARGET-IDX) TO ITEM-TYPE(MOVE-IDX)
               MOVE ITEM-VALUE(TARGET-IDX) TO ITEM-VALUE(MOVE-IDX)
               MOVE ITEM-QUANTITY(TARGET-IDX) TO ITEM-QUANTITY(MOVE-IDX)
               MOVE ITEM-EQUIPPED(TARGET-IDX) TO ITEM-EQUIPPED(MOVE-IDX)
               MOVE ITEM-ATTACK(TARGET-IDX) TO ITEM-ATTACK(MOVE-IDX)
               MOVE ITEM-DEFENSE(TARGET-IDX) TO ITEM-DEFENSE(MOVE-IDX)
               MOVE ITEM-HEAL(TARGET-IDX) TO ITEM-HEAL(MOVE-IDX)
               MOVE ITEM-MANA(TARGET-IDX) TO ITEM-MANA(MOVE-IDX)
           END-PERFORM
      
           SUBTRACT 1 FROM INVENTORY-COUNT
           .
      
      *-----------------------------------------------------------------
      * Affichage de l'inventaire
      *-----------------------------------------------------------------
       DISPLAY-INVENTORY.
           DISPLAY SPACE
           DISPLAY "=== INVENTAIRE ==="
           DISPLAY "Or: " PLAYER-GOLD
           DISPLAY "Objets: " INVENTORY-COUNT "/" MAX-INVENTORY-SIZE
           DISPLAY SPACE
      
           PERFORM VARYING INV-IDX FROM 1 BY 1 
                   UNTIL INV-IDX > INVENTORY-COUNT
      
               DISPLAY INV-IDX ". " WITH NO ADVANCING
      
               IF IS-EQUIPPED(INV-IDX)
                   DISPLAY "[E] " WITH NO ADVANCING
               ELSE
                   DISPLAY "    " WITH NO ADVANCING
               END-IF
      
               DISPLAY ITEM-NAME(INV-IDX) WITH NO ADVANCING
      
               IF ITEM-QUANTITY(INV-IDX) > 1
                   DISPLAY " x" ITEM-QUANTITY(INV-IDX) WITH NO ADVANCING
               END-IF
      
               EVALUATE ITEM-TYPE(INV-IDX)
                   WHEN "W"
                       DISPLAY " - Arme (ATT+" 
                           ITEM-ATTACK(INV-IDX) ")" 
               END-EVALUATE
      
               DISPLAY SPACE
           END-PERFORM
      
           DISPLAY SPACE
           DISPLAY "Équipement:"
           PERFORM VARYING EQUIP-IDX FROM 1 BY 1 
                   UNTIL EQUIP-IDX > MAX-EQUIPMENT-SLOTS
               DISPLAY EQUIP-TYPE(EQUIP-IDX) ": " WITH NO ADVANCING
               IF EQUIP-ITEM-ID(EQUIP-IDX) = 0
                   DISPLAY "(Aucun)"
               ELSE
                   PERFORM VARYING INV-IDX FROM 1 BY 1 
                           UNTIL INV-IDX > INVENTORY-COUNT
                       IF ITEM-ID(INV-IDX) = EQUIP-ITEM-ID(EQUIP-IDX)
                           DISPLAY ITEM-NAME(INV-IDX)
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
      
       END PROGRAM INVENTORY-SYSTEM.