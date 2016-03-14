exec: snake
	./snake

snake: snake.ml
	ocamlc unix.cma graphics.cma snake.ml -o snake