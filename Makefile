OCAMLOPT    = ocamlopt
EXERCISES   = ex00 ex01 ex02 ex03 ex04

# Colors
DEF     = \033[0m
BOLD        = \033[1m
RED     = \033[1;91m
GREEN       = \033[1;92m
YELLOW      = \033[1;93m
CYAN        = \033[1;96m
PINK        = \033[38;5;206m
ORANGE      = \033[38;5;214m
SAND        = \033[38;5;222m
BLUE        = \033[38;5;117m

all: $(EXERCISES) camel

$(EXERCISES):
	@echo "$(CYAN)$(BOLD)Compiling $@...$(DEF)"
	@$(OCAMLOPT) $@.ml -o $@ \
		&& echo "$(GREEN)  [ OK ] $@$(DEF)" \
		|| echo "$(RED)  [FAIL] $@$(DEF)"

camel:
	@echo ""
	@echo "$(SAND)$(BOLD)  '\|/'                                  $(YELLOW)*$(DEF)"
	@echo "$(SAND)$(BOLD) -- $(YELLOW)*$(SAND) -----$(DEF)"
	@echo "$(SAND)$(BOLD)   /|\\      ____$(DEF)"
	@echo "$(SAND)$(BOLD)  ' | '    {_   o^>       $(YELLOW)*$(DEF)"
	@echo "$(SAND)$(BOLD)    :        -_  /)$(DEF)"
	@echo "$(SAND)$(BOLD)    :         (   (        $(GREEN).-''\`'.$(DEF)"
	@echo "$(SAND)$(BOLD)    .          \\   \\      $(GREEN)/       \ \$(DEF)"
	@echo "$(SAND)$(BOLD)    .           \\    \\   $(GREEN)/         \ \$(DEF)"
	@echo "$(SAND)$(BOLD)                 \\    \`-'$(GREEN)           \`'.$(DEF)"
	@echo "$(SAND)$(BOLD)                  \\    . '        /    \`.$(DEF)"
	@echo "$(SAND)$(BOLD)                   \\  ( \\  )     (     .')$(DEF)"
	@echo "$(SAND)$(BOLD)    ,,   t          '. |  /       |     ($(DEF)"
	@echo "$(SAND)$(BOLD)   '|\`\`_/^\\___        '|  |\`'-..-'|   ( ()$(DEF)"
	@echo "$(BLUE)$(BOLD)_~~|~/_|_|__/|~~~~~~~~ |  / ~~~~~ |   | ~~~~~~~~$(DEF)"
	@echo "$(SAND)$(BOLD) -_  |L[|]L|/          | |\\ $(GREEN) 42 $(SAND)  )   )$(DEF)"
	@echo "$(BLUE)$(BOLD)                       ( |(       /  /|$(DEF)"
	@echo "$(BLUE)$(BOLD)   ~~ ~  ~~ ~~~~       | /\\\      / /| |$(DEF)"
	@echo "$(BLUE)$(BOLD)                       ||  \\\\   _/ / | |$(DEF)"
	@echo "$(SAND)$(BOLD)             ~ ~~ ~~~ _|| (_/ (___)_| |$(DEF)"
	@echo "$(SAND)$(BOLD)                     (__)         (____)$(DEF)"
	@echo ""
	@echo "$(BOLD)$(CYAN)<><><><><><><><><><><><><><><><><><><><><><><><><><><>$(DEF)"
	@echo "$(BOLD)$(GREEN)  Enter the Matrix compiled! Run 'make run' to test.$(DEF)"
	@echo "$(BOLD)$(CYAN)<><><><><><><><><><><><><><><><><><><><><><><><><><><>$(DEF)"
	@echo ""

run:
	@for ex in $(EXERCISES); do \
		if [ -f $$ex ]; then \
			echo "$(PINK)$(BOLD)=== $$ex ===$(DEF)"; \
			./$$ex; \
			echo ""; \
		fi \
	done

clean:
	@echo "$(RED)$(BOLD)Cleaning compiled files...$(DEF)"
	@rm -f *.cmo *.cmi *.cmx *.cmxa *.o *.a

fclean: clean
	@echo "$(RED)$(BOLD)Removing executables...$(DEF)"
	@rm -f $(EXERCISES)

re: fclean all

.PHONY: all run clean fclean re $(EXERCISES) camel
