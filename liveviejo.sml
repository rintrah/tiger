structure tigerliveness  =
struct

open tigerassem
open tigertemp
open tigermap 
open tigerset

fun upto m n = if m > n then [] else m :: upto (m+1) n 

fun equalMap (mapa_1, mapa_2) = 
	let	fun f ((ind_1,conj_1 ),(ind_2, conj_2 )) = equalSet(conj_1,conj_2)
	in 
			ListPair.all f (listItemsMap mapa_1, listItemsMap mapa_2)
	end		

fun use(OPER{src, ...}) = fromListSet(src, tempeq)
	| use(LABEL{...}) = newSet tempeq
	| use (MOVE{src, ...}) = fromListSet([src], tempeq)
	

fun def(OPER{dst, ...}) = fromListSet(dst, tempeq)
	| def(LABEL{...}) = newSet tempeq
	| def (MOVE{dst, ...}) = fromListSet([dst], tempeq)

fun Liveness block = 
	let 
			val (In, In') =(newMap Int.compare, newMap Int.compare)
			val ( Out, Out') = (newMap Int.compare, newMap Int.compare)
			val indices = upto 1 (List.length block)
			val instr = ListPair.zip (indices, block)
			val _ = List.app (fn x => insertMap In x (newSet tempeq)) indices
			val _ = List.app (fn x => insertMap In' x (newSet tempeq)) indices
			val _ = List.app (fn x => insertMap Out x (newSet tempeq)) indices
			val _ = List.app (fn x => insertMap Out' x (newSet tempeq)) indices

			fun sucesor (i, OPER{jump=SOME [etiqueta], ...}) = 
				let val next = if i < (List.length block) then [i+1] else []
						fun isLabel (_, LABEL{lab, ...}) = (labelAstring lab) = (labelAstring etiqueta)
								|isLabel _ = false
						val salto = case List.find isLabel instr of SOME (i, _) => [i]
														| NONE => raise Fail "etiqueta no encontrada.\n"
				in
					print("Los sucesores de "^Int.toString(i)^" son ");
					List.app (fn x=>print (Int.toString(x)^ ", ")) (salto@next);
					print("\n");
					salto@next
				end
			| sucesor (i, _)  = if i < List.length block then [i+1] else [] before
				(
					print("Los sucesores de "^Int.toString(i)^" son "^Int.toString(i+1));
					print("\n")
				)
			fun iteracion(i, instruccion) =
				let
(*					val _ = print("iterando con "^ Int.toString(i)^ "\n");*)
					val _ = insertMap Out' i (getMap Out i)
					val _ = insertMap In' i (getMap In i)
					val _ = insertMap In i (unionSet(use(instruccion), minusSet(getMap Out i, def(instruccion))))
					val _ = let val t = newSet tempeq 
									in List.app (fn x => addSet(t, getMap In x)) (sucesor (i, instruccion));
										 insertMap Out i t	
									end
				in	
						()
				end
			
			fun puntoFijo() = (List.app iteracion instr;
												if (equalMap(In, In') andalso equalMap (Out,Out')) then () 
												else puntoFijo())
			in
				puntoFijo();Out
			end

end
