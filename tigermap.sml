structure tigermap :> tigermap =
struct
		open Splaymap
		
		type ('key, 'a) tigermap = ('key, 'a) dict ref

		fun newMap cmp= ref (mkDict cmp)

		fun insertMap m llave elem = m:= insert(!m, llave, elem)

		fun findMap m llave neutro = (case peek(!m, llave) of SOME e => e
																				| NONE => neutro)

		fun peekMap m llave = peek(!m, llave)


		fun numItemsMap m = numItems(!m)

		fun listItemsMap m = listItems(!m)

		fun appMap f m = app f (!m)
		
		fun forAllMap f m = appMap f m

	
		fun printMap f g  m = List.app (fn (x,y) => print("["^(f x)^"] => "^(g y)^" \n")) (listItemsMap m)   
		
		fun getMap m llave = find(!m, llave) 
		
		fun setEmptyMap cmp mapa =  mapa:=  (mkDict) cmp
		
end

	
	 	
