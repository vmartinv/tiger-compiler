# compiler
This is an implementation of a Tiger compiler following the book "Modern Compiler Implementation in ML" by Andrew W. Appel and the instructions of Guido Macchi (and assistance from Guillermo Grinblat and Martín Ceresa) for a Computer Science course. Tiger is a simple, statically-typed programming language with nested functions first introduced in the aforementioned book.

## Authors
* Aldana Ramirez
* Lautaro Rinaldi
* Martín Villagra

## Extensions
Besides the basic parts of the compiler the following features were implemented:

### includes
Include a file inside the let construction. Example:
```
let
  include "other_definitions.tigd"
in
  0
end
```
This feature was implemented by adding an extra stage just after parsing, where includes are replaced for the contents of the included files. Extra care was taken for detecting include loops. Implementation lives on `tigerinclude.sml`.


We ended up with 164 tests for the tiger compiler.


### extern
Added an `extern` keyword to declarate extern functions, this enables calling C functions from Tiger or to use any external library. Example:
```
let
  type SDL_Window = {}
  type SDL_Surface = {}
  extern function SDL_GetWindowSurface(win:SDL_Window):SDL_Surface
in
  SDL_GetWindowSurface(nil);
  0
end
```
It was implemented by only editing the parser (`tigergrm.y`) as the compiler was already handling `extern` functions.


### gcc flags
All unrecognized flags are passed to gcc, which generates the executable code on the last stage. This enables passing parameters to the linking process, which is specially useful for projects requiring libraries. This is implemented in `tigermain.sml`.

Example:
```
./tiger ../juego/main.tig -lSDL2 -lSDL2_ttf -lSDL2_image -lSDL2_mixer ../juego/lib.c
```


### optimization 1: Representation of sets
During liveness analysis, following the suggestion from the book on page 216, we represent sets as ordered lists which drastically reduced the time of that step. This can be seen on `tigerliveness.sml`.


### optimization 2: Ordering the nodes
During liveness analysis, we implemented the optimization mentioned in the book on page 389. This greatly reduces compilation time by topological sorting the flow graph and use the order to process the nodes, thus reaching the fixed point much faster. Implemented on `tigerliveness.sml`.


### test automatization
We developed a simple script to run and maintain a test suite. Each test was added manually, it consists of a code and the expected output of the compiler and its execution. We do a small parsing of the first line of the code of the test in order to embed what parameters should be passed to the compiler (for example to print intermediate trees). This enables to create and maintain the test suite even when not all stages of the compiler are finished. It has saved us a lot of time by detecing bugs early and to avoid introducing new ones. The script is `tests/test` and is written in Bash.

We ended up with 164 tests for the tiger compiler.

Example test:

> liveness/hello.tig
```
/*TIGER_ARGS=-code -flow*/
(print ("Hello World");0)
```

> liveness/hello.out
```bien!
;;--FRAME--L0__tigermain_0:
LABEL: L3
MOVE: movq %'s0, %'d0 D:T2 S:rbx
MOVE: movq %'s0, %'d0 D:T3 S:r12
...
	n22(LABEL: L2): n23
	n23(OPER:  D:[] S:[rax,rsp,rbp,rbx,r12,r13,r14,r15]): 
;;-END-FLOW-:
yes!!
Hello World
Return code: 0
```

Screenshot:

![Screenshot of tests running](https://raw.githubusercontent.com/vmartinv/compiler/master/screenshots/test_script.png)


### sokoban game
By using some of the features implemented small game was programmed. It uses `SDL 2` C library and function wrappers in `juego/lib.c` has been written to call functions with strings (as C strings are diferent from Tiger strings). You can run it by:
1. Installing SDL2 (with image, mixer and TTF libs).

  For Ubuntu:
  
    follow this link https://gist.github.com/BoredBored/3187339a99f7786c25075d4d9c80fad5
   
  For Arch Linux:
  
    sudo pacman -S sdl2 sdl2_image sdl2_mixer sdl2_ttf
  
2. Run in a terminal:
  > cd juego
  
  > cd make run


Screenshot:

![Screenshot of first level of the game](https://raw.githubusercontent.com/vmartinv/compiler/master/screenshots/juego.png)

### compiler pipeline
We encapsulated each stage of the compiler in a function and by defining the operator `>>=` we are able to write the entire compiler as a series of steps in a very clear way:

```
fun perFragment fragment = 
    fragment >>= instructionSel >>= prntCode >>=
        debugLivenessAnalysis
        >>= coloreo
        >>= prntColor
        >>= procExit3
        >>= formatter

source_filename >>= abreArchivo >>=
   lexer >>= parser >>= (*de ASCII al arbol tigerabs.exp*)
   expIncludes >>=  (*etapa agregada para que funcionen los includes*)
   escap >>= prntArbol >>= 
   seman >>= prntIr >>= (*chequeo de tipos y generacion de fragmentos*)
   canonize >>= prntCanon >>=
   (fn (stringList, frags) => (stringList, map perFragment frags)) >>=
   serializer >>= prntAsm >>=
   compileAsm >>=
   prntOk (*si llega hasta aca esta todo ok*)
```


### file opener
All source files start with `tiger` and there are many files with the same name but different extensions (some come from compilation subproducts). For this reason we found ourselves spending a lot of time looking for the specific files we wanted to open. For this reason we made a small script to list and open the sources files easily. Implementation is in `src/e` and is written in Bash.

Screenshot:

![Screenshot of file opener](https://raw.githubusercontent.com/vmartinv/compiler/master/screenshots/e_script.png)

## Compiler stages
  A diagram was created showing the main stages of the compiler.
  https://drive.google.com/file/d/0ByI9GfoY63_aTDFaUjVLRjVwM1k/view?usp=sharing



## Geany extensions (for coloring ML files):
Run in a terminal:
>  mkdir -p ~/.config/geany/colorschemes && wget https://labdcc.fceia.unr.edu.ar/~mvillagra/filetype_extensions.conf -O ~/.config/geany/filetype_extensions.conf && curl -O https://labdcc.fceia.unr.edu.ar/~mvillagra/geany.colors.tar.gz && tar -zxf geany.colors.tar.gz -C ~/.config/geany/colorschemes/ && rm geany.colors.tar.gz



## Requeriments
* mosml (ML compiler)

  For Ubuntu:
    > sudo apt-get install libgmp-dev
    
    > wget https://github.com/kfl/mosml/archive/ver-2.10.1.tar.gz
    
    > tar zxf ver-2.10.1.tar.gz
    
    > cd mosml-ver-2.10.1/src
    
    > make
    
    > sudo make install
  
  For Arch Linux:
    > sudo yaourt -S mosml



## Links útiles
* Pagina para aprender git:
  https://git-scm.com/book/es/v1
  
* SVN de la materia:
  svn checkout https://svn.dcc.fceia.unr.edu.ar/svn/lcc/T-521-Compiladores/
  
* Entregas iniciales + cosas:
  git clone https://bitbucket.org/martinceresa/tiger-compiler
