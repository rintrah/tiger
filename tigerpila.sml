structure tigerpila :> tigerpila =
struct
	type 'a Pila = ('a list) ref 
	

	fun nuevaPila () = ref []
	fun nuevaPila1 x = ref [x]
	fun pushPila x y = x := y::(!x)
	fun popPila x = x:=List.tl(!x)
		handle Empty => raise Fail "La pila no tiene m�s que un elemento"
	fun topPila x = hd(!x)
		handle Empty => raise Fail "La pila no tiene ning�n elemento"

	fun showPila x = !x

end
