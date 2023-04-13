structure tigerreg  =
struct
		
		open tigerframe	
		open tigertemp
		open tigerassem
		open tigerset
		open tigermap 
		open tigerpila
		open tigerliveness

fun coloreo(marco:tigerframe.frame, instrucciones:tigerassem.instr list) =
let
		val bloqueInstrucciones = ref instrucciones 
		val nuevoMarco = ref marco
		val listaDeRegistros = [ rv, ov, ebx, ecx, esi, edi, fp, sp]  

		fun itoa i = (if i < 0 then "-" else "")^(Int.toString(Int.abs(i))) 
		fun temptupla((a,b), (c,d)) =
			 case  (tempeq(a,c), tempeq (b,d)) of
				(EQUAL, EQUAL) => EQUAL
				|(EQUAL, e)  => e
				| (e, _) => e

 		fun cmpInstr((a,b), (c,d)) = Int.compare(a,c)
	
		(* Conjuntos, listas globales para el coloreo. *)
		(* Lista globales de moves. *)

		val worklistMoves = newSet cmpInstr 
		val activeMoves = newSet cmpInstr
		val coalescedMoves = newSet cmpInstr
		val constrainedMoves = newSet cmpInstr
		val frozenMoves = newSet cmpInstr
		
		(*  Listas de trabajo, conjuntos y pilas de nodos. *)

		val precolored = fromListSet(listaDeRegistros, tempeq);

		val initial = newSet tempeq
		val simplifyWorkList = newSet tempeq
		val freezeWorkList = newSet tempeq
		val spillWorkList = newSet tempeq
		val spilledNodes = newSet tempeq
		val coalescedNodes = newSet tempeq
		val coloredNodes = newSet tempeq
		val selectStack:tigertemp.temp tigerpila.Pila = nuevaPila ()
		val pushTemp = tigerpila.pushPila selectStack
		fun showTemp () = tigerpila.showPila selectStack 
		fun popTemp () = tigerpila.popPila selectStack
		fun topTemp () = tigerpila.topPila selectStack

		(* Otras estructuras de datos. *)

		val K = 8 
		val adjList = newMap tempeq
		val adjSet = newSet temptupla
		val alias = newMap tempeq
		val degree = newMap tempeq
		val _ = List.app 
						(fn x => (insertMap degree x  
							(case Int.maxInt of SOME e => e
							| NONE =>  raise Fail "¡Error: mosml no tiene entero máximo!")
							))
							listaDeRegistros 

		val color = newMap tempeq
		val moveList = newMap tempeq

		(* Funciones auxiliares para el coloreo. *)
	
		fun upto(m,n) = if m > n then [] else m :: upto (m+1, n)
		
		val _ = 
			let val l = ListPair.zip (upto(0,(List.length listaDeRegistros)-1), 
										listaDeRegistros) 
			in
				 List.app (fn (x,y) => insertMap color y x ) l 
			end


    fun isMoveInstruction (OPER {...})  = false
    |isMoveInstruction (LABEL {...})    = false
    |isMoveInstruction (MOVE {...})     = true

    fun use (OPER {src, ...}) = fromListSet(src, tempeq)
    |use (LABEL{...}) = newSet tempeq
    |use (MOVE{src, ...}) = fromListSet([src], tempeq)

    fun def (OPER {dst, ...}) = fromListSet(dst, tempeq)
    |def (LABEL{...}) = newSet tempeq
    |def (MOVE{dst,...}) = fromListSet([dst], tempeq)

    val _ = (List.app (fn x => (addSet(initial, use x);
						 addSet(initial, def x))) (!bloqueInstrucciones);
            differenceSet(initial, precolored))

		fun printSets () =
					(print ("\nInitial: \n"^printSet(initial, tempAstring)^"\n"); 
					 print ("SimplifyWorkList: \n"^printSet(simplifyWorkList, tempAstring)^"\n"); 
					 print ("FreezeWorkList: \n"^printSet(freezeWorkList, tempAstring)^"\n");
					 print ("SpillWorkList: \n"^printSet(spillWorkList, tempAstring)^"\n");
					 print ("SpilledNodes: \n"^printSet(spilledNodes, tempAstring)^"\n"); 
					 print ("CoalescedNodes: \n"^printSet(coalescedNodes, tempAstring)^"\n");
					 print ("ColoredNodes: \n"^printSet(coloredNodes, tempAstring)^"\n"); 
					 print ("SelectStack: \n") ;List.app (fn x=> print (tempAstring(x)^"\n")) (showTemp());
					 print "\n")	

		fun printAdjSet () = print (printSet(adjSet, fn (x,y) => ("("^tempAstring(x)^","^tempAstring(y)^")")))		

		val single = singletonSet tempeq

		fun encuentraMapa mapa llave neutro = case peekMap mapa llave  of 
																					SOME a => a 
																					| NONE => neutro 

		fun AddEdge(u,v) =
			if (not(memberSet(adjSet,(u,v)))	andalso (cmptemp(u,v) = false)) then
				(insertSet(adjSet, (u,v)); insertSet(adjSet,(v,u));
					if not(memberSet(precolored, u)) then
						(insertMap adjList u (unionSet(encuentraMapa adjList u (newSet tempeq), single v));
						 insertMap degree u ((encuentraMapa degree u 0)+1))
					else ();
					if not(memberSet(precolored, v)) then
						(insertMap adjList v (unionSet(encuentraMapa adjList v (newSet tempeq), single u));
						 insertMap degree v ((encuentraMapa degree v 0)+1))
					else ())
			else ()
	
		fun Adjacent n =
			let val nuevoSet = fromListSet(showTemp(), tempeq);
			in 
				case (peekMap adjList n) of
					SOME e => minusSet(e , unionSet(nuevoSet, coalescedNodes)) 
					|NONE  => (newSet tempeq)
			end
		
		fun NodeMoves n =
			case (peekMap moveList n) of 
				SOME e=> interSet(e ,unionSet(activeMoves, worklistMoves))
				|NONE => newSet(cmpInstr)  
				
		fun MoveRelated n = not(isEmptySet(NodeMoves n))	

		fun EnableMoves nodes =
			forAllSet(nodes, 
			fn n => 
				forAllSet(NodeMoves n, 
					fn m => if (memberSet(activeMoves, m))
					then
						(differenceSet(activeMoves, singletonSet cmpInstr m);
					  addSet(worklistMoves, singletonSet cmpInstr m))
					else ())) 

		fun DecrementDegree m =
				let 
						val d = case peekMap degree m of
									SOME e => e 
							    | NONE  => raise Fail ("El temporal "^tempAstring(m)^" no tiene grado\n")
						val _ =  insertMap degree m (d - 1)
				in
 					if d = K then 
								(EnableMoves(unionSet(single m, (Adjacent m)));
								differenceSet(spillWorkList, single m);
								if (MoveRelated m) then 
										addSet(freezeWorkList, single m)
								else addSet(simplifyWorkList, single m))
					else ()
				end 

		fun AddWorkList u =
			if not(memberSet(precolored, u)) andalso
				 not((MoveRelated u) andalso 
					(((getMap degree u )
				 handle NotFound => raise Fail ("Not found surgió en AddWorkList con el temporal "^tempAstring(u)))  < K ))
				 then
						(differenceSet(freezeWorkList,single u);
						addSet(simplifyWorkList,single u))
				else ()  
		
		fun Ok(t,r) =
		 let val valor = 
				case (peekMap degree t ) of SOME e => e
				| NONE => raise Fail ("El temporal "^(tempAstring t)^" no tiene grado, para la función Ok\n.")
		 in
				valor < K orelse memberSet(precolored, t)
				orelse memberSet(adjSet, (t, r))
		 end 

		fun Conservative nodes =
			let val k = ref 0
					fun grado n  =
					 case peekMap degree n of 
						SOME e => e
						|NONE => raise Fail ("El temporal "^(tempAstring n)^" no tiene grado, para la función Conservative.\n") 
			in 
				forAllSet(nodes,
					fn n => if grado n >= K 
						then k := !k + 1
						else ());
				!k < K
			end 

		fun GetAlias n =
			if memberSet(coalescedNodes, n)
				 then (
					 case peekMap alias n of
						SOME e => (GetAlias e)
						| NONE => raise Fail ("El temporal "^(tempAstring n)^" no tiene alias, para la función GetAlias")) 
			else n 

		fun Combine(u,v) = 
			(if memberSet(freezeWorkList, v) 
			then
				differenceSet(freezeWorkList, single v)
			else 
				differenceSet(spillWorkList, single v);
			addSet(coalescedNodes, single v);
			insertMap alias v  u;
			insertMap moveList u (unionSet(encuentraMapa moveList u (newSet cmpInstr), getMap moveList v));
			EnableMoves(single v);
			forAllSet(Adjacent(v), fn t =>
				 (AddEdge(t, u); DecrementDegree(t)));
			if ((getMap degree u )
					 handle NotFound => raise Fail ("El temporal "^tempAstring(u)^" no tiene grado.\n"))
				 >= K andalso memberSet(freezeWorkList, u) 
			then
				(differenceSet(freezeWorkList, single u); 
				addSet(spillWorkList, single u))
			else ())

		fun FreezeMoves (u) = 
			forAllSet(NodeMoves(u),
				fn m =>
				let  	
					fun fuente(_ , MOVE{src, ...}) = src
					|fuente _ = raise Fail "Existe una instrucción no move en worklistMoves"
					fun destino(_, MOVE{dst, ...}) = dst
					|destino _ = raise Fail "Existe una instrucción no move en worklistMoves" 
					val (x,y) = (fuente(m), destino(m))	
					val v = if cmptemp(GetAlias(y),(GetAlias(u))) 
									then 
											GetAlias(x)
									else 
											GetAlias(y)
				in
					differenceSet(activeMoves, singletonSet cmpInstr m);
					addSet(frozenMoves, singletonSet cmpInstr m);
					if isEmptySet(NodeMoves(v)) andalso 
						((getMap degree v )
								 handle NotFount => raise Fail ("El temporal "^tempAstring(v)^" no tiene grado en Freeze.\n"))
						< K then 
						(differenceSet(freezeWorkList, single v); 
						addSet(simplifyWorkList, single v))
					else ()
				end )

		fun Invariant() =
				(forAllSet(unionSet(simplifyWorkList,unionSet(freezeWorkList, spillWorkList)),
					fn u => if (getMap degree u = cardinalSet(interSet(getMap adjList u,unionSet(precolored, unionSet(simplifyWorkList,
				unionSet(freezeWorkList, spillWorkList)))))) then () 
					else raise Fail ("El invariante de Degree no se cumple en el temporal "^tempAstring(u)^"\n")); 
				forAllSet(spillWorkList, 
					fn u => if (getMap degree u >= K) then ()
					else raise Fail ("El invariante de Spill WorkList no se cumple en el temporal "^tempAstring(u)^"\n"))) 					
 
		(* Funciones principales del coloreo. *)

		fun Build () =
				let
						val listilla = 
							ListPair.zip (upto(1,List.length (!(bloqueInstrucciones))),
							!(bloqueInstrucciones))
						val listaInstr = List.rev listilla
						val liveOut = Liveness (!(bloqueInstrucciones))
				(*		val _ = printMap Int.toString (fn n => printSet(n, tempAstring)) liveOut *)
						val live = 	newSet tempeq
				in
				List.app 
					(fn  (ind, I) =>	
						let 
								val (useI, defI) = (use I , def I) 
								val useDef = unionSet(useI, defI)		
								val _ =	case I of
												OPER{jump = SOME [etiqueta], ...}  =>  
												(setEmptySet(live);
													addSet(live,
		(getMap liveOut ind) handle NotFound => raise Fail ("No se encontró el liveOut de "^Int.toString(ind)^"\n")))
												| _ => ()  	
						in
					    if isMoveInstruction I then 
									(differenceSet(live, useI);
									forAllSet (useDef, fn x => 
									case (peekMap moveList x) of
									SOME a => insertMap moveList x (addSet(getMap moveList x,singletonSet cmpInstr (ind, I));getMap moveList x)
									|NONE => insertMap moveList x (singletonSet cmpInstr (ind, I)));
									addSet(worklistMoves, singletonSet cmpInstr (ind,I)))
									else ();
									addSet(live, defI);
									forAllSet (defI, fn x => forAllSet(live, fn y => AddEdge(x, y)));
									differenceSet(live, defI);
									addSet(live, useI) 
						end) listaInstr		
		end
														
		fun MakeWorklist () =
			forAllSet(initial, 
				fn n => (differenceSet(initial, single n);
								if (encuentraMapa degree n 0) >= K 
								then 
									addSet(spillWorkList, single n)
								else if MoveRelated(n)
										 then 
										 addSet(freezeWorkList, single n)
										 else
										 addSet(simplifyWorkList, single n);
				())) 
		
		fun Simplify	() =
				let
						val n = List.hd (listItemsSet simplifyWorkList)
            val _ = differenceSet(simplifyWorkList,single n)
						val _ = pushTemp n;	
				in
  					forAllSet(Adjacent n, fn m => DecrementDegree m)
				end 
	
		fun Coalesce () =
			let
				fun fuente(_ , MOVE{src, ...}) = src
				|fuente _ = raise Fail "Existe una instrucción no move en worklistMoves"
				fun destino(_, MOVE{dst, ...}) = dst
				|destino _ = raise Fail "Existe una instrucción no move en worklistMoves" 
				val m = List.hd (listItemsSet worklistMoves)
				val x = GetAlias(destino m)
				val y = GetAlias(fuente m)
				val (u,v) = if memberSet(precolored, y) then (y,x) else (x,y)
			in
				differenceSet(worklistMoves,singletonSet cmpInstr m); 
				if cmptemp(u,v) 
				then (
					addSet(coalescedMoves,singletonSet cmpInstr m);
					AddWorkList(u))
				else if (memberSet(precolored, v) orelse memberSet(adjSet, (u,v)))
						 then
						(addSet(constrainedMoves, singletonSet cmpInstr m);
						 AddWorkList(u);
						 AddWorkList(v))
				else if memberSet(precolored, u) andalso 
							(List.all (fn t => Ok(t,u)) (listItemsSet(Adjacent(v)))) orelse
							not(memberSet(precolored, u))	 andalso 
							Conservative(unionSet(Adjacent u, Adjacent v)) then
							(addSet(coalescedMoves, singletonSet cmpInstr m);
							Combine(u,v);
							AddWorkList(u)) 
						else
							addSet(activeMoves, singletonSet cmpInstr m)
			end 
	  
		
		fun Freeze () =
			let val u = List.hd (listItemsSet(freezeWorkList))
			in
				differenceSet(freezeWorkList,single u);
				addSet(simplifyWorkList, single u); 
				FreezeMoves(u)
			end
	
		fun SelectSpill () =
			let val m = List.hd(listItemsSet(spillWorkList))
			in
				differenceSet(spillWorkList, single m);
				addSet(simplifyWorkList, single m); 
				FreezeMoves(m)
			end

		fun AssignColor () =
			let 
				val _ = while (not(null(showTemp()))) do 
					let val n = topTemp();
							val _ = popTemp();
							val okColors = 
								let val t= newSet(Int.compare)
						    		val _ = addSet(t, fromListSet([0,1,2,3,4,5], Int.compare))
										in t 
								end
					in 
						forAllSet(encuentraMapa adjList n (newSet tempeq),
						fn w => if (memberSet(unionSet(coloredNodes, precolored), GetAlias(w)))
										then differenceSet(okColors, singletonSet Int.compare 
										(getMap color (GetAlias(w))
								 handle NotFound => raise Fail ("NotFound surgió en color GetAlias debido al temporal"^tempAstring(w))
										))
									 else ());
					 if (isEmptySet(okColors)) 
					 then addSet(spilledNodes, single n)
					 else 
								let  val L = listItemsSet(okColors)
									 	 val c = List.hd(L)
								in 
										addSet(coloredNodes, single n); 
										insertMap color n c 
								end
					end
			in
				forAllSet(coalescedNodes,
				fn n => insertMap color n (
							(getMap color (GetAlias (n)) 
			handle NotFound => 
			raise Fail ("Surgió un error en Assign Color debido al temporal: "
					^tempAstring(n)^" y su alias: "^tempAstring(GetAlias(n))^".\n")
			)))
		end 		 

		
		fun  iter()= if (not(isEmptySet(simplifyWorkList))) then Simplify ()
						else if (not(isEmptySet(worklistMoves))) then Coalesce ()
						else if (not(isEmptySet(freezeWorkList))) then Freeze ()
						else if (not(isEmptySet(spillWorkList))) then SelectSpill () else ()			 

		fun RewriteProgram () = 
			let 
			(*	val _ = printSets()
				val _ = printMap tempAstring (fn n => printSet(n,tempAstring)) adjList*)
				val newTemps = newSet(tempeq)	

				fun use(reg, OPER{src, ...}) = 
						List.exists (fn x => cmptemp(reg, x)) src
				|use(reg, MOVE{src, ...}) = cmptemp(reg, src)
				|use(reg, LABEL{...}) = false 

				fun definition(reg, OPER{dst, ...}) = 
						List.exists (fn x => cmptemp(reg, x)) dst
				|definition(reg, MOVE{dst, ...}) = cmptemp(reg, dst)
				|definition(reg, LABEL{...}) = false
				
				fun cambiarDestino(reg, temporal, OPER{assem, dst, src, jump}) = 
					OPER{assem=assem,  
						dst = List.map (fn x => if cmptemp(x, reg) then temporal else x) dst,
						src=src, jump=jump}
				|cambiarDestino(reg, temporal, MOVE{assem, src, dst}) = 
				  MOVE{assem=assem, dst=temporal, src=src}
				|cambiarDestino _ = 
					raise Fail "Error: Se trata de cambiar el campo dst en un LABEL.\n"
		
				fun cambiarFuente(reg, temporal, OPER{assem, dst, src, jump}) =
					OPER{assem=assem, dst=dst, 
						src=List.map (fn x => if cmptemp(x, reg) then temporal else x) src,
					jump = jump}
				|cambiarFuente(reg, temporal, MOVE{assem, dst, src}) = 
					MOVE{assem=assem, dst=dst, src=temporal}
				|cambiarFuente _ = raise Fail "Error: Se trata de cambiar el campo src en un LABEL.\n"
	
				fun fetch (posicion, temporal) =
					OPER{assem = "movl "^itoa(posicion)^"(%`s0), %`d0 #91\n",
					dst = [temporal], src = [fp], jump=NONE}
				fun store (posicion, temporal) =
					 OPER{assem = "movl %`s0, "^itoa(posicion)^"(%`s1) #92\n",
					dst = [], src = [temporal,fp], jump=NONE}
	
			fun reescritura (v,pos, []) = [] 
			|reescritura (v,pos, x::xs) = 
				if (use(v, x)) then 
					let  
						val temporal = newtemp()
						val _ = addSet(newTemps, single temporal)
					in
						fetch(pos, temporal)::
							cambiarFuente(v, temporal,x)::
							reescritura(v,pos, xs)
					end
				else if (definition(v, x)) then
					let
						val temporal = newtemp()
						val _ = addSet(newTemps, single temporal)
					in
						cambiarDestino(v, temporal, x)::
							store(pos, temporal)::
							reescritura(v,pos, xs)
					end
				else x::(reescritura(v,pos, xs))

			fun help (v, lista) =
				let
					val pos = case  (allocLocal  (!nuevoMarco) true) of InFrame e => e
					|InReg e => raise Fail "Error en la alocación de RewriteProgram\n"
				in
					reescritura (v, pos, lista)
				end		
			val _ = bloqueInstrucciones:=  List.foldr help (!bloqueInstrucciones) (listItemsSet spilledNodes)
			val _ = setEmptySet(spilledNodes)
			val _ = setEmptySet(initial)
			val _ = addSet(initial, coloredNodes)
			val _ = addSet(initial, coalescedNodes)
			val _ = addSet(initial, newTemps) 	
			val _ = setEmptySet(coloredNodes)
			val _ = setEmptySet(coalescedNodes)
		in
			()	
		end
	
		fun Colorear() = 
			let
				fun registro x =
					List.nth (listaDeRegistros, (getMap color x )
					 handle NotFound => raise Fail ("No se encontró el color del temporal "^tempAstring(x) ^" \n"))
				fun pintar(OPER{assem, dst, src,  jump}) =
					 OPER{assem=assem, dst=List.map registro dst,
						src=List.map registro src, jump=jump}
				|pintar(MOVE{assem, dst, src}) = MOVE{assem=assem, dst=registro dst, src=registro src}
				|pintar e = e
			in
				bloqueInstrucciones:= List.map pintar(!bloqueInstrucciones)
			end

		fun Coalescer() = 
				let 
						fun eliminar(MOVE{assem, dst, src}) = if (cmptemp(dst,src)) then false else true
						|eliminar _ = true
				in 
						(bloqueInstrucciones:=List.filter eliminar (!bloqueInstrucciones))
 				end

		fun repeat() = (
			iter();  
			(if ((isEmptySet(simplifyWorkList)) andalso (isEmptySet(worklistMoves)) andalso (isEmptySet(freezeWorkList)) andalso
			(isEmptySet(spillWorkList))) then () else repeat()))

		fun principal() =
			(Build();
			MakeWorklist (); 
			repeat();
			AssignColor();
		  if isEmptySet(spilledNodes) then () else 
			(RewriteProgram();principal()))

			 
	
in
	principal();
	Colorear(); 
	Coalescer();
	procEntryExit3(!nuevoMarco,!bloqueInstrucciones)
	 
end
			
end
  	
