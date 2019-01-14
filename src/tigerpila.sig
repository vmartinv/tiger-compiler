signature tigerpila =
sig

type 'a Pila
val nuevaPila : unit -> 'a Pila
val nuevaPila1 : 'a -> 'a Pila
val push : 'a Pila -> 'a -> unit
val pop : 'a Pila -> unit
val top : 'a Pila -> 'a
val isEmpty : 'a Pila -> bool

end
