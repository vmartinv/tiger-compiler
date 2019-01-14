structure tigerpila :> tigerpila =
struct

type 'a Pila = 'a list ref
fun nuevaPila() = ref []
fun nuevaPila1 e = ref [e]
fun push pila item = pila:=(item::(!pila))
fun pop pila =
    let val ret = hd(!pila)
    in pila:=tl(!pila) end
fun top pila = hd(!pila)
fun isEmpty pila = null(!pila)

end
