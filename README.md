# compiler
This is an implementation of a Tiger compiler following the book "Modern Compiler Implementation in ML" by Andrew W. Appel and the instructions of Guido Macchi (and assistance from Guillermo Grinblat and Martín Ceresa) for a Computer Science course. Tiger is a simple, statically-typed programming language with nested functions first introduced in the aforementioned book.



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
This feature was implemented by adding an extra stage just after parsing, where includes are replaced for the contents of the included files. Extra care was taken for detecting include loops.


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
It was implemented by only editing the parser (tigergrm.y) as the compiler was already handling `extern` functions.


### gcc flags
All unrecognized flags are passed to gcc, which generates the executable code on the last stage. This enables passing parameters to the linking process, which is specially useful for projects requiring libraries.


### Optimization 1: Representation of Sets
During liveness analysis, following the suggestion from the book on page 216, we represent sets as ordered lists which drastically reduced the time of that step. This can be seen on `tigerliveness.sml`


### Optimization 2: Ordering the nodes
During liveness analysis, we implemented the optimization mentioned in the book on page 389. This greatly reduces compilation time by topological sorting the flow graph and use the order to process the nodes, thus reaching the fixed point much faster.


### Sokoban Game
By using some of the features implemented small game was programmed. You can run it by:
1. Installing SDL2 (with image, mixer and TTF libs).
  For Ubuntu:
    follow this link https://gist.github.com/BoredBored/3187339a99f7786c25075d4d9c80fad5
   
  For Arch Linux:
    > sudo pacman -S sdl2 sdl2_image sdl2_mixer sdl2_ttf
2. Running:
  > cd juego
  > cd make run



## Compiler stages
  This diagram shows the main stages of the compiler.
  https://drive.google.com/file/d/0ByI9GfoY63_aTDFaUjVLRjVwM1k/view?usp=sharing



## Geany extensions (for coloring ML files):
Run in a terminal:
>  mkdir -p ~/.config/geany/colorschemes && wget https://labdcc.fceia.unr.edu.ar/~mvillagra/filetype_extensions.conf -O ~/.config/geany/filetype_extensions.conf && curl -O https://labdcc.fceia.unr.edu.ar/~mvillagra/geany.colors.tar.gz && tar -zxf geany.colors.tar.gz -C ~/.config/geany/colorschemes/ && rm geany.colors.tar.gz



## Requeriments
* mosml (ML compiler)

  For Ubuntu:
    follow this link https://gist.github.com/BoredBored/3187339a99f7786c25075d4d9c80fad5
   
  For Arch Linux:
    > sudo pacman -S mosml



## Links útiles
* Pagina para aprender git:
  https://git-scm.com/book/es/v1
  
* SVN de la materia:
  svn checkout https://svn.dcc.fceia.unr.edu.ar/svn/lcc/T-521-Compiladores/
  
* Entregas iniciales + cosas:
  git clone https://bitbucket.org/martinceresa/tiger-compiler
