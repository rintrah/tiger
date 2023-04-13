structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs
open tigerit

val labelPila: (tigertemp.label option) tigerpila.Pila =
		tigerpila.nuevaPila()
val pushLabel = tigerpila.pushPila labelPila
fun popLabel() = tigerpila.popPila labelPila
fun topLabel() = tigerpila.topPila labelPila 

datatype exp = Ex of tigertree.exp
		|Nx of tigertree.stm
		|Cx of tigertemp.label*tigertemp.label->tigertree.stm
		|Ux of unit 

fun init [] = raise Fail ("Lista vacía")
| init [x] = []
| init (x::y::xs) = x::init(y::xs)

fun seq l = List.foldr (fn (x,y) => SEQ(x,y)) (List.last l) (init l) 
		handle Empty => raise Fail "Atención lista vacía para seq"

fun unEx(Ex e) = e
| unEx(Nx s) = ESEQ(s, CONST 0)
| unEx(Cx c) =
	let val r = newtemp()
	    val (t,f) =(newlabel(), newlabel())
	in 
	    ESEQ(
		seq[MOVE(TEMP r, CONST 1),
		c(t,f),
		LABEL f, MOVE (TEMP r, CONST 0),
		LABEL t],
		TEMP r)
	end
| unEx _ = raise Fail "Ocurrió un error en traslate en unEx"

fun unNx (Ex e) = EXP e 
| unNx (Nx s) = s
| unNx(Cx c) = EXP (unEx (Cx c))
| unNx _ = raise Fail "Ocurrió un error en translate en unNx"

fun unCx (Ex (CONST 0)) = (fn (t,f) => JUMP(NAME f, [f]))
| unCx (Ex (CONST _)) = (fn (t,f) => JUMP (NAME t, [t]))
| unCx (Ex e ) = (fn (t,f) => CJUMP(NE, e, CONST 0, t, f))
| unCx (Cx c) =  c
| unCx _ = raise Fail "Ocurrió un error en translate en unCx"

fun imprime e = 
	let val s = tree(unNx e)
	in
	print s
end


 
type level = {parent:frame option , frame: frame, level: int}
type access = tigerframe.access
type frag = tigerframe.frag
val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
val outermost: level = {parent=NONE,
	frame=newFrame{name="_tigermain", formals=[]}, level=0}
fun newLevel({parent, frame, level}:level, name, formals:bool list) = {
	parent=SOME frame,
	frame=newFrame{name=name, formals=formals},
	level=level+1}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
fun formals{parent, frame, level} = tigerframe.formals frame
fun getActualLev() = !actualLevel

fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a completar!"
		fun aux2(PROC{body, frame}) = aux(Nx body)
		| aux2(STRING(l, "")) = (labelAstring l)^":\n"
	(*	| aux2(STRING("", s)) = "\t"^s^"\n" *)
		| aux2(STRING(l, s)) = (labelAstring) l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	aux3 e end
fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")

val datosGlobs = ref ([]: frag list)

fun procEntryExit{level: level, body} =
	let	val label = STRING(stringAlabel(tigerframe.name(#frame(level))),"")
		val body' = PROC{frame= #frame level, body=unNx body}
		val final = STRING(stringAlabel(";;-------"),"")
	in	datosGlobs:=(!datosGlobs@[label, body', final]) end 

fun getResult() = !datosGlobs

fun stringLen s =
	let	fun aux[] = 0
		| aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
		| aux(_::t) = 1+aux(t)
	in	aux(explode s) end

fun stringExp(s: string) =
	let	val l = newlabel()
		val len = ".long "^makestring(stringLen s)
		val str = ".string \""^s^"\""
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING(stringAlabel(""), str)])
	in	Ex(NAME l) end

fun preFunctionDec(l, n) =
	(pushLabel(NONE);
	actualLevel := !actualLevel+1;
	newLevel(l,n,[]))

fun functionDec(e, l, proc) =
	let
		val body =
			let
				val lt = List.map (fn _ => newtemp()) calleesaves
				val lt' = ListPair.zip(lt, calleesaves)
				val pre = List.map (fn(t,r) => MOVE(TEMP t, TEMP r)) lt'
				val post = List.map (fn(t,r) => MOVE(TEMP r, TEMP t)) lt'
			in
				seq(pre@
					[if proc then unNx e else MOVE(TEMP rv, unEx e)]
					@post)
			end
		val body' = procEntryExit1(#frame l, body)
		val () = procEntryExit{body=Nx body', level=l}
	in	Ex(CONST 0) end
 
fun postFunctionDec() =
	 (popLabel(); actualLevel := !actualLevel-1)

fun intExp n = Ex(CONST n)

fun nilExp () = Ex (CONST 0) 

fun unitExp ()= Nx (EXP (CONST 0))

fun preWhileFor ()  =  pushLabel( SOME (newlabel()))
fun postWhileFor () =  popLabel ()

fun whileExp(test,body) =
	let val test' = unCx test
	    val body' = unNx body 
	    val (init, t) =(newlabel(), newlabel())
	    val f = (case (topLabel()) of
			SOME e => e
			|_ =>  raise Fail  "Error el label del While es NONE"
			handle Empty => raise Fail "Error la lista de labels está vacía")
	in 
		Nx(seq [LABEL init,
		test'(t, f),
		LABEL t,
		body',
		JUMP(NAME init, [init]),
		LABEL f])
	end

fun forExp(v, lo, hi, body) =
	let val fin = newtemp()
	    val t = newlabel()
	    val f = (case ( topLabel()) of 
			SOME e => e
			| _ => raise Fail "Error el label del For es NONE"
			handle Empty => raise Fail "Error la lista de labels está vacía")	
	    val v' = unEx v
	in
		Nx(seq[MOVE (v', unEx lo),
		       	MOVE (TEMP fin, unEx hi),
			CJUMP (LE, v', TEMP fin, t, f),
			LABEL t,
			unNx body,
			MOVE (v', BINOP (PLUS, v', CONST 1)),
			CJUMP (LE ,v', TEMP fin, t, f),
			LABEL f])
end
			
	
fun breakExp () = (case  (topLabel()) of 
			SOME e => Nx (JUMP (NAME e, [e]))
			| _=> raise Fail "El label del Break es NONE"
			handle Empty => raise Fail "Error la lista de labels está vacía")
	

fun ifThenExp (test, body) =
	let 
		val (t,f) = (newlabel(), newlabel())
		val test' = unCx test
		val body' = unNx body
	in
		Nx (seq[test'(t,f),
			LABEL t,
			body',
			LABEL f])
end

fun ifThenElseExp (test,body, body') =
	let 
		val (t,f,salida) = (newlabel(), newlabel(), newlabel())
		val test' = unCx test
		val r =newtemp()
	in
		Ex( ESEQ((seq[test' (t,f),
			LABEL t,
			MOVE (TEMP r, unEx body),
			JUMP (NAME salida, [salida]),
			LABEL f,
			MOVE (TEMP r, unEx body'),
			LABEL salida]), TEMP r))
end

fun ifThenElseExpUnit (test, body, body') =
	let 
		val (t,f,salida) = (newlabel(), newlabel(),newlabel())
		val test' = unCx test
	in
		Nx(seq[test'(t,f),
			LABEL t,
			unNx body,
			JUMP (NAME salida, [salida]),
			LABEL f,
			unNx body,
			LABEL salida])
end

fun seqExp l =
	let 
		val lista = List.map unNx (init l)
		val final = unEx (List.last l)
	in 
		Ex (ESEQ (seq lista, final))
end

fun assignExp (expvar, expass) = Nx (MOVE (unEx expvar, unEx expass))

fun incluir a 0 = []
| incluir a n = a ::(incluir a (n-1)) 


fun letExp (inits, body) = if List.null inits then Ex (unEx body)
				else Ex (ESEQ (seq inits, unEx body))
 
fun binOpIntExp (explf, oper, expr) =
	let fun checkOp PlusOp = PLUS
		|checkOp MinusOp = MINUS
		|checkOp TimesOp = MUL
		|checkOp DivideOp = DIV
		|checkOp _ = raise Fail "El operando no corresponde a una operación aritmética"
	  
		val tr = newtemp()
		
		fun aux (CONST a, CONST b, operando) = (case operando of 
																						PLUS => Ex (CONST (a+b))
																						| MINUS => Ex (CONST (a-b))
																						| MUL => Ex (CONST (a*b))
																						| DIV => Ex (CONST (a div b))
																						| _ => raise Fail "Operando desconocido en operación aritmética")
			| aux (CONST a, b, operando) = let val t1 = newtemp()
																		 in 
																					Ex(ESEQ(seq [MOVE (TEMP t1, b),
																						MOVE (TEMP tr, BINOP(operando, CONST a, TEMP t1))],
																						TEMP tr))
																			end
			| aux (a, CONST b, operando) = let val t1 = newtemp()
																		 in
																					Ex(ESEQ(seq [MOVE (TEMP t1, a),
																						MOVE (TEMP tr, BINOP (operando, TEMP t1, CONST b))],
																						TEMP tr))
																		 end
			| aux (a, b, operando) = let val (t1, t2) = (newtemp(), newtemp())
															 in
																			Ex(ESEQ(seq[ MOVE (TEMP t1, a),
								 												MOVE (TEMP t2, b),
								 												MOVE (TEMP tr, BINOP (operando, TEMP t1 , TEMP t2))],
								 												TEMP tr))
															end
		in
			aux(unEx explf, unEx expr, checkOp oper)
end

fun binOpStrExp (explf, oper, expr) =
	let fun checkOp EqOp = EQ 
		|checkOp NeqOp = NE
		|checkOp LtOp = LT
		|checkOp LeOp = LE
		|checkOp GtOp = GT
		|checkOp GeOp = GE
		|checkOp _ = raise Fail "El operando no corresponde a una operación de comparación de strings"

	     val (t1, t2, tr)  =(newtemp(), newtemp(), newtemp())
	in
		Ex(ESEQ(seq [MOVE (TEMP t1, unEx explf),
								MOVE (TEMP t2, unEx expr),
								EXP(CALL (NAME (stringAlabel "_stringEqual"), [TEMP t1, TEMP t2])),
								MOVE (TEMP tr, TEMP rv)],
						TEMP tr))
end

fun binOpIntExpEq (explf, oper, expr) =
	let fun checkOp EqOp = EQ 
		|checkOp NeqOp = NE
		|checkOp LtOp = LT
		|checkOp LeOp = LE
		|checkOp GtOp = GT
		|checkOp GeOp = GE
		|checkOp _ = raise Fail "El operando no corresponde a una operación de comparación de enteros"
	     val r =newtemp()
	     val (t, f) = (newlabel(), newlabel())
	in
		Cx (fn (t,f) => CJUMP (checkOp oper, unEx explf, unEx expr,t, f))
end


fun subscriptVar (v,i) =
	let val t1 = newtemp()
	    val t2 = newtemp()
	in Ex(ESEQ (seq [MOVE (TEMP t1, unEx v),
			MOVE (TEMP t2, unEx i),
			EXP (CALL (NAME (stringAlabel "_checkindex"), [TEMP t1, TEMP t2]))],
			MEM (BINOP (PLUS, BINOP(MUL, TEMP t2, CONST wSz), TEMP t1))))
end 

fun varDec (vexp,rexp) =
	(MOVE(unEx vexp, unEx rexp))


fun arrayExp (s, i) =
	let val ts = newtemp()
	    val ti = newtemp()
	    val tr = newtemp()
	in
		Ex (ESEQ(seq[MOVE (TEMP ts, unEx s),
				MOVE (TEMP ti, unEx i),
				MOVE (TEMP tr,ESEQ( EXP (CALL  (NAME (stringAlabel "_newArray"), [TEMP ts, TEMP ti])), TEMP rv))],
				TEMP tr))
end

fun recordExp lexp =
	let  fun aux  e = 
		let val t = newtemp()
		in ESEQ(MOVE (TEMP t, unEx e), TEMP t) end
	     val tr = newtemp()
	in
	     Ex (ESEQ (seq [EXP (CALL (NAME (stringAlabel "_newRecord"),(CONST (List.length lexp))::(List.map aux lexp))), MOVE (TEMP tr, TEMP rv)], TEMP tr))
end

fun fieldVar(r, m) =
	let val tr = newtemp()
	    val tm = newtemp()
in
	Ex(ESEQ ( seq [MOVE (TEMP tr, unEx r),
		EXP (CALL (NAME (stringAlabel "_checkNil"), [TEMP tr]))],
		MEM (BINOP (PLUS, TEMP tr,CONST (m * wSz)))))
end		 

fun callExp(func,ret,level:level, largs, extern) =
let
	val nivel = #level level - getActualLev() 
	val arg0 = case nivel of
			1 => (TEMP fp)
			|0 => MEM( BINOP (PLUS, TEMP fp, CONST ( 2* wSz)))
			|_ =>  (let 
					val t= newtemp()
					val n = (abs  nivel) - 1   
					val a = (MOVE (TEMP t, MEM(BINOP (PLUS,TEMP t, CONST(2*wSz)))))
				in 
					ESEQ(seq ((MOVE(TEMP t, MEM(BINOP(PLUS,TEMP fp, CONST (2*wSz)))))::(incluir a n)), (TEMP t))
				end)
					
	fun aux e = 
		let val t = newtemp()
		in ESEQ(MOVE (TEMP t, unEx e), TEMP t)
		end
	val tr = newtemp()
in
	if extern then 
		Ex (ESEQ(SEQ(EXP(CALL (NAME (stringAlabel func), (map aux largs))),
				MOVE(TEMP tr, TEMP rv)), TEMP tr))
	else	
		if ret then
			Ex  (ESEQ(SEQ(EXP (CALL (NAME (stringAlabel func), arg0::(map aux largs))),
					MOVE (TEMP tr, TEMP rv)), TEMP tr))
			else
					Nx (EXP(CALL (NAME (stringAlabel func), arg0::(map aux largs)))) 
end
	
fun simpleVar(acc, level) =
	(case acc of 
		InReg t => Ex(TEMP t) 
		| InFrame off => if level=getActualLev() then
					Ex( MEM(BINOP (PLUS, TEMP fp,CONST off)))
				else 
					let 
						val t =newtemp()
						val a = (MOVE (TEMP t, MEM(BINOP (PLUS,TEMP t, CONST(2*wSz)))))
					in
						Ex(ESEQ(seq ((MOVE(TEMP t, MEM(BINOP(PLUS,TEMP fp, CONST (2*wSz)))))::(incluir a (Int.abs(level -getActualLev())-1))), MEM (BINOP(PLUS, TEMP t, CONST off))))
					end)
end
