##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

NAME	= pushswap

CC	= ghc

RM	= rm -f

SRCS	= bonus.hs

all: $(NAME)

$(NAME): $(OBJS)
	 $(CC) -o $(NAME) $(SRCS)

clean:
	$(RM) *.o
	$(RM) *.hi

fclean: clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all clean fclean re tests_run
