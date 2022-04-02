##
## EPITECH PROJECT, 2017
## Makefile
## File description:
## makefile of the source file
##

NAME	=	imageCompressor

CC	=	stack

all:	$(NAME)

$(NAME):
	@$(CC) build
	@cp `stack path --local-install-root`/bin/$(NAME)-exe ./$(NAME)

clean:
	@$(CC) clean

fclean: clean
	@rm -rf ./$(NAME)

re:	fclean all

.PHONY: fclean clean all re debug