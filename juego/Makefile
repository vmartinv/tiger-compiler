main : main.tig lib.c sdl.tigd sounds.tigd
	cd ../src; \
	./tiger ../juego/main.tig ../juego/lib.c -no-pie `sdl2-config --cflags --libs` -lSDL2 -lSDL2_ttf -lSDL2_image -lSDL2_mixer

run : main tiles_packed.png yoster.ttf
	./main

clean :
	rm -f main
