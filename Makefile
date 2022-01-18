NAME	=	funEvalExpr
package	=	funEvalExpr

stack_yaml	=	STACK_YAML='stack.yaml'
stack		=	$(stack_yaml) stack
local_path	:=	$(shell stack path --local-install-root)
executable	:=	$(local_path)/bin

all: $(NAME)

$(NAME):
	$(stack) build $(package)
	cp $(executable)/funEvalExpr-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) -f $(NAME)

re: fclean all

tests_run:
	stack test

.PHONY: all $(NAME) fclean clean re tests_run
