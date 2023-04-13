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

			fun sucesor (i, OPER{jump=SOME [etiqueta], assem=s,...}) = 
				let 
						val jmpstring = String.substring(s,0,3)
						val next = if i < (List.length block) andalso jmpstring<>"jmp" then [i+1] else []
						fun isLabel (_, LABEL{lab, ...}) = (labelAstring lab) = (labelAstring etiqueta)
								|isLabel _ = false
						val salto = case List.find isLabel instr of SOME (i, _) => [i]
														| NONE => raise Fail "etiqueta no encontrada.\n"
				in
				(*	print("Los sucesores de "^Int.toString(i)^" son ");
					List.app (fn x=>print (Int.toString(x)^ ", ")) ( if (ListPair.all (op =) (salto,next)) then salto else salto@next) ;
					print("\n");*)
					if (ListPair.all (op =) (salto,next)) then salto else salto@next
				end

			| sucesor (i, _)  = (if i < List.length block then [i+1] else [])
				(*
					print("Los sucesores de un no jump "^Int.toString(i)^" son "^Int.toString(i+1));
					print("\n")
				*)

			fun iteracion(i, instruccion) =
				let
					val _ = insertMap Out' i (getMap Out i)
					val _ = insertMap In' i (getMap In i)
					val defs = def(instruccion)
					val uses = use(instruccion)
					val outi = getMap Out i
					val ini = getMap In i
					val suci = sucesor(i,instruccion)
	
					val _ = insertMap In i ( unionSet( uses, minusSet(outi, defs)))
					val t = newSet tempeq 

				in 
					List.app (fn x => addSet(t, (getMap In x))) suci;

				(*	print ("Los use de "^makestring(i)^" ");
				  print	(printSet (uses,tempAstring));
					print ("\n");

					print ("Agregando a los outs de "^makestring(i)^" ");
				  print	(printSet (t,tempAstring));
					print ("\n");*)
 				 	insertMap Out i t	
				end
			
			fun puntoFijo() = (List.app iteracion instr;
												if (equalMap(In, In') andalso equalMap (Out,Out')) then () 
												else puntoFijo())
			in
				puntoFijo();Out
			end

end
