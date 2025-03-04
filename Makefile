# Makefile pour COBOLegend - RPG textuel en COBOL

# Compilateur et options
COBC = cobc
COBC_FLAGS = -x -Wall

# Nom de l'exécutable
EXE = cobolegend

# Fichiers sources
SRC_FILES = MAIN-GAME.cbl CHARACTER.cbl COMBAT.cbl WORLD.cbl QUEST.cbl INVENTORY.cbl UI.cbl UTILITIES.cbl TERMINAL-TIME-TRAVEL.cbl DIALOG-MULTIPLEXER.cbl

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

# Compilation du module Terminal Time Travel uniquement
time-travel: TERMINAL-TIME-TRAVEL.cbl
	$(COBC) $(COBC_FLAGS) -o terminal-time-travel $<

# Compilation du module de dialogue uniquement
dialog: DIALOG-MULTIPLEXER.cbl
	$(COBC) $(COBC_FLAGS) -o dialog-multiplexer $<

.PHONY: all clean run time-travel dialog
