exec: snake.ml
	./snake

snake.ml: snake.ml
	ocamlc unix.cma graphics.cma snake.ml -o snake