main : main.tig lib.c sdl.tigd sounds.tigd
	cd ../src; \
	./tiger ../juego/main.tig -lSDL2 -lSDL2_ttf -lSDL2_image -lSDL2_mixer ../juego/lib.c

run : main tiles_packed.png yoster.ttf
	./main

clean :
	rm -f main
