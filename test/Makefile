##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

NAME	= pushswap_checker

CC	= ghc

RM	= rm -f

SRCS	= Test.hs

all: $(NAME)

$(NAME): $(OBJS)
	 $(CC) -o $(NAME) $(SRCS)

clean:
	$(RM) *.o
	$(RM) *.hi

fclean: clean
	$(RM) $(NAME)

test:
	runhaskell $(SRCS)

re: fclean all

.PHONY: all clean fclean re tests_run
