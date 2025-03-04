# Makefile pour COBOLegend - RPG textuel en COBOL

# Compilateur et options
COBC = cobc
COBC_FLAGS = -x -Wall

# Nom de l'exécutable
EXE = cobolegend

# Fichiers sources
SRC_FILES = MAIN-GAME.cbl CHARACTER.cbl COMBAT.cbl WORLD.cbl QUEST.cbl INVENTORY.cbl UI.cbl UTILITIES.cbl

# Règle par défaut
all: $(EXE)

# Compilation du programme principal
$(EXE): $(SRC_FILES)
	$(COBC) $(COBC_FLAGS) -o $@ $^

# Nettoyage
clean:
	rm -f $(EXE)

# Lancement du jeu
run: $(EXE)
	./$(EXE)

.PHONY: all clean run
