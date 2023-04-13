structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertab
open tigermisc
open topsort
open listpp 
open tigertrans
open tigerpila

val mainLevel=tigertrans.outermost
val levelPila:tigertrans.level tigerpila.Pila =
	tigerpila.nuevaPila1(mainLevel)
val pushLevel = tigerpila.pushPila levelPila
fun popLevel() = tigerpila.popPila levelPila
fun topLevel() = tigerpila.topPila levelPila 

fun error(mens, pos)=
	raise Fail ("error: "^mens^" en "^Int.toString(pos)) 

type expty = {exp:exp, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla


val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString)])

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=outermost, label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=outermost, label="flush",
		formals=[], result=TUnit, extern=true}),
	("getstr", Func{level=outermost, label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=outermost, label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=outermost, label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=outermost, label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=outermost, label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=outermost, label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=outermost, label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=outermost, label="exit",
		formals=[TInt], result=TUnit, extern=true})
	])
	
fun comparar (a, TInt) (b, TInt) = true
|comparar   (a, TString) ( b, TString) = true
|comparar (a, TTipo (c, r)) (b, TTipo (d, s)) =
	 if r=s then true else false  
|comparar (a, TTipo (c, r)) (b, d) =
	let
		val u = case r of
			ref (SOME e) => e
			|ref NONE  =>TUnit
	in
  	comparar (a, u) (b, d)
	end
|comparar (a, c) (b, TTipo (d, r)) =
	let
		val u = case r of
	  	ref (SOME e) => e
			|ref NONE  =>TUnit
	in
		comparar (a, c) (b, u)
	end
|comparar (a, TNil) (b, TRecord _) = true
|comparar (a, TRecord _) (b, TNil) = true
|comparar (a, TArray (c, u1)) (b, TArray (d, u2)) =
	if u1 = u2 then comparar (a,c) (b,d) else false
|comparar (a, TRecord (c, u1)) (b, TRecord (d, u2))  = 
	if u1 = u2 then  true else false 
|comparar _ _ = false

fun igual (TRecord _ ) (TNil ) = true
|igual (TNil ) (TRecord _ ) = true 
|igual (TRecord (_, u1)) (TRecord (_, u2 )) = 
	if u1 = u2 then true else false
|igual (TArray (_, u1)) (TArray (_, u2)) = 
	if u1 = u2 then true else false
|igual (TTipo (_, r)) b = 
	let 
		val a = case r of
		ref (SOME e) => e
		|ref NONE => TUnit
	in
		igual a b
	end
|igual a (TTipo (_,r)) =
	let 
		val b = case r of
		ref (SOME e) => e
		|ref NONE => TUnit
	in
		igual a b
	end
|igual a  b = 
	if a = b then true else false 

fun transExp(venv, tenv) =
	let fun trexp(VarExp v) = trvar v

			|trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}

			|trexp(NilExp _)= {exp=nilExp(), ty=TNil}

			|trexp(IntExp(i, _)) = {exp=intExp i, ty=TInt}

			|trexp(StringExp(s, _)) = {exp=stringExp s, ty=TString}

			|trexp(CallExp({func=f, args}, nl)) =
				let
					val (flevel, flabel, fargs, fret,fextern) =
						case tabBusca(f, venv) of
							SOME (Func{level, label, formals, result, extern}) => (level, label, formals, result, extern)
							|SOME _=> error ("No es una funcion", nl)
							| NONE => error("Nombre desconocido", nl)
					val expList = List.map (fn x => trexp x ) (args)
					val (fargs', fargs'') = ListPair.unzip  (List.map (fn x => (#ty x, # exp x)) expList)
			    val temp = ListPair.map (uncurry igual) (fargs, fargs')
			    fun igualdad  [] = true
			    |igualdad (x::xs) = x andalso (igualdad xs)
			    val temp' = igualdad temp
			    val tret = (case fret of TUnit => false
							| _ => true)
			in 
				if (List.length fargs = length fargs') 
			  then 
					if  temp' 
					then {exp =callExp(flabel,tret,flevel,fargs'',fextern), ty=fret} 
					else error 
						("Los argumentos que se pasan a la función, no tienen el mismo tipo que los declarados en la declaración de la función", nl)
			    else error
						 ("La función que se desea llamar tiene más o menos argumentos que los que se le está pasando", nl) 
		 	 end  	

			|trexp(OpExp({left, oper=EqOp, right}, nl)) = 
				let val {exp=explft, ty=lfty} = trexp left
			  	  val {exp=exprht, ty=rty}  = trexp right
				in 
			  	if (igual lfty rty) then
					 (case (lfty, rty) of
						(TString, TString) => {exp=binOpStrExp(explft, EqOp, exprht), ty= TInt}
						| _ =>  {exp=binOpIntExpEq(explft,EqOp,exprht), ty =TInt}) 
					else  error ("Los operandos que se desea comparar por igualdad, tienen distinto tipo", nl) 
				end 	 

			|trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
				let val {exp=explft, ty=lfty} = trexp left
			  		val {exp=exprht, ty=rty}  = trexp right
				in 
  				if (igual lfty rty) then 
						(case (lfty, rty) of
							(TString, TString) => {exp=binOpStrExp(explft, NeqOp, exprht), ty =TInt}
							| _ => {exp=binOpIntExpEq(explft,NeqOp,exprht),ty =TInt})
					else error ("Los operandos que se desea comparar por desigualdad, tienen distinto tipo", nl)
				end

			|trexp(OpExp({left, oper, right}, nl)) = 
				let val {exp=explf, ty=lfty} = trexp left
			  		val {exp=expr, ty=rty}  = trexp right
			    
				in 
			  	case oper of
						PlusOp => (if lfty=TInt andalso rty=TInt
											 then {exp=binOpIntExp(explf, oper, expr), ty=TInt}
			 								 else error ("Los operandos de una suma deben tener tipo entero", nl))
						|MinusOp => (if lfty=TInt andalso rty=TInt
											 	then {exp=binOpIntExp(explf, oper, expr), ty=TInt}
			   		  				 	else error ("Los operandos de una resta deben tener tipo entero", nl))
						|TimesOp => (if lfty=TInt andalso rty=TInt
												then {exp=binOpIntExp(explf, oper, expr), ty=TInt}
			  		  					else error ("Los operandos de una multiplicación deben tener tipo entero", nl))
						|DivideOp => (if lfty=TInt andalso rty=TInt
												 then {exp=binOpIntExp(explf, oper, expr), ty=TInt}
					  						 else error ("Los operandos de una división deben tener tipo entero", nl))
						|LtOp => (case (lfty, rty) of 
											(TInt, TInt) => {exp=binOpIntExpEq(explf, oper, expr), ty =TInt}
											|(TString, TString) => {exp=binOpStrExp(explf, oper, expr), ty= TInt}
											| _ => error ("Los operandos en una operación  <  son de tipo entero o strings", nl))
						|LeOp => (case (lfty, rty) of 
											(TInt, TInt) => {exp=binOpIntExpEq(explf, oper, expr), ty =TInt}
											|(TString, TString) => {exp=binOpStrExp(explf, oper, expr), ty= TInt}
											| _ => error ("Los operandos en una operación  <=  son de tipo entero o strings", nl))
						|GtOp => (case (lfty, rty) of 
											(TInt, TInt) => {exp=binOpIntExpEq(explf, oper, expr), ty =TInt}
											|(TString, TString) => {exp=binOpStrExp(explf, oper, expr), ty= TInt}
											| _ => error ("Los operandos en una operación  >  son de tipo entero o strings", nl))
						|GeOp => (case (lfty, rty) of 
											(TInt, TInt) => {exp=binOpIntExpEq(explf, oper, expr), ty =TInt}
											|(TString, TString) => {exp=binOpStrExp(explf, oper, expr), ty= TInt}
											| _ => error ("Los operandos en una operación  >=  son de tipo entero o strings", nl))
						| _ => error ("Error del parser, la operación no pertenece a la sintaxis abstracta de Tiger", nl)
				end

			|trexp(RecordExp({fields, typ}, nl)) =
				let val {campos=rCampos, unico=rUnico} = 
							case tabBusca(typ, tenv) of
							 SOME (TRecord (e, unico))   => {campos=e, unico=unico}
							 |SOME e => error("No es un Record", nl)
							 |NONE => error("No es un tipo", nl)
			    	val elementos= List.map (fn (x,y,z) => (x,y)) rCampos
			    	val elementos' = quick elementos
						val campExp = List.map (fn (x,y) => (x, transExp (venv, tenv) y)) fields
						val fields' = quick (List.map (fn (x,y) => (x, #ty y)) campExp)	
			    	val temp = ListPair.map (uncurry comparar) (fields', elementos')	
			    	fun verificar [] = true
			    	|verificar (x::xs) = x andalso (verificar xs)
			    	val valor = verificar temp
						val exprecord = List.map (fn (x, y) => #exp(y)) campExp
				in
			  	if List.length fields' = List.length elementos' andalso valor 
					then {exp=recordExp exprecord, ty= TRecord (rCampos, rUnico)} 
				  else error ("Faltan/sobran elementos", nl)
				end

			|trexp(SeqExp(s, nl)) =
				let val lexti = map trexp s
			  	  val exprs = map (fn{exp, ty} => exp) lexti
			    	val {exp, ty=tipo} = hd(rev lexti)
							handle Empty => raise Fail "Lista vacía en una secuencia"
				in	
        	{exp=seqExp exprs, ty=tipo }
        end

			|trexp(AssignExp({var=SimpleVar s, exp=assign}, nl)) =
				let val {exp=expvar, ty=tyvar} = trvar (SimpleVar s, nl)
			  	  val {exp=expass, ty=tyexp} = trexp assign 
			    	val _ = case tabBusca(s, venv) of
											SOME(Var{ty, readOnly, access, level}) =>
											 if readOnly then error("Se trata de asignar una variable sólo de lectura", nl)
											 else ()
					| _ => error("El nombre no es una variable o no se encuentra en el entorno", nl) 
				in
			    if igual tyvar tyexp
					then {exp=assignExp(expvar,expass), ty=TUnit} 
			    else error("Tipos diferentes", nl)
				end

			|trexp(AssignExp({var, exp=assing}, nl)) =
				let val {exp=expvar, ty=tyvar} = trvar (var, nl)
			  	  val {exp=expass, ty=tyexp} = trexp assing
				in
			  	if igual tyvar tyexp
					then {exp=assignExp(expvar, expass), ty=TUnit} 
			    else error("Expresion de tipo incorrecto", nl)
				end

			|trexp(IfExp({test, then', else'=SOME else'}, nl)) =
				let val {exp=exptest, ty=tytest}  = trexp test
			   	 val {exp=expthen, ty=tythen'} = trexp then'
			    	val {exp=expelse', ty=tyelse'} = trexp else'  
				in 
			  	if tytest=TInt andalso igual tythen' tyelse' 
					then {exp=ifThenElseExp(exptest,expthen,expelse'), ty=tythen'}
			    else error ("Error de tipos en el if", nl)
			end

			|trexp(IfExp({test, then', else'=NONE}, nl)) =
				let val {exp=exptest, ty=tytest}  = trexp test
			  	  val {exp=expbody, ty=tythen'} = trexp then' 
				in 
			    if tytest=TInt andalso tythen'=TUnit 
					then {exp=ifThenExp(exptest,expbody), ty=TUnit}
			    else error ("Error de tipos en el if", nl)
				end

			|trexp(WhileExp({test, body}, nl)) =
				let val {exp=exptest, ty=tytest} = trexp test
			   	  val _ = preWhileFor () 
		        val {exp=expbody, ty=tybody} = trexp body
				in
			    if tytest = TInt  
					then if tybody = TUnit 
							 then {exp=whileExp(exptest,expbody), ty=TUnit} 
							 	before postWhileFor ()  
							 else error("El cuerpo no tiene tipo TUnit", nl)                           
				 	else error ("Condicion del while de tipo incorrecto", nl)
				end    

			|trexp(ForExp({var, escape, lo, hi, body}, nl)) =
				let val {exp=explo, ty=tylo} = trexp lo
			  	  val {exp=exphi, ty=tyhi} = trexp hi
			    	val venv'= 
							tabInserta (var,Var{ty=TInt,readOnly=true,
							access=allocLocal (topLevel()) (!escape) , level=nl}, venv)  
			    	val _ = preWhileFor ()
			    	val {exp=expbody, ty=tybody} =  transExp (venv', tenv) body
			    	val {exp=expvar, ty=tyvar}  = transExp (venv', tenv) (VarExp ((SimpleVar var),nl))  
				in
			  	if tylo=TInt andalso tyhi=TInt
					then if tybody=TUnit 
							 then {exp=forExp(expvar,explo,exphi,expbody), ty=tybody} before postWhileFor ()
							 else error ("El cuerpo del for no es de tipo TUnit", nl)
			   	else error ("Los indices son de tipo diferente", nl)
				end 

			|trexp(LetExp({decs, body}, _)) =
				let fun f((v,t,ls), d) =
						let
							val (v', t', ls') = transDecs (v,t) d
						in 
							(v',t', ls@ls') 
						end
			    	val (venv', tenv',ls) = List.foldl  (f o flip) (venv, tenv,[]) decs
			    	val {exp=expbody, ty= tybody} = transExp (venv', tenv') body
				in 
			    {exp=letExp(ls,expbody),ty=tybody} 
				end

			|trexp(BreakExp nl) = {exp=breakExp(), ty=TUnit}
			
			|trexp(ArrayExp({typ, size, init}, nl)) =
				let val {exp=expsize, ty=tysize} = trexp size
			  	  val {exp=expinit, ty=tyinit} = trexp init
			    	val (tarr, u) =
					 	 case tabBusca(typ, tenv) of
							SOME (TArray (t,u))=> (t,u)
							|SOME e => error("La variable no es un array", nl)
							|NONE =>  error("Variable no definida", nl)
				in
					if tysize <> TInt then error("Tamaño no entero!", nl) 
					else if igual tyinit tarr
						 then {exp=arrayExp(expsize, expinit), ty=TArray(tarr,u)}
						 else error("Tipo incorrecto de init", nl)
				end

		and  trvar(SimpleVar s, nl) =
			(case tabBusca(s, venv) of
			  SOME(Var{ty=tyvar,readOnly, access=varacc,level=varlevel}) =>
				{exp=simpleVar(varacc, varlevel),ty=tyvar}
			 | _ => error("Variable no definida", nl))

		| trvar(FieldVar(v, s), nl) =
			let val {exp=expvar, ty=tyvar} = trvar (v, nl) 
			    val lat = case tyvar of
					TRecord (tatrs, _) => tatrs
					|TTipo(n, r)=>(case r of ref (SOME e) => (case e of TRecord (tatrs, _) => tatrs
										 | _ => error("No es un Record", nl))
							|_=> error("No es un Record", nl))  
					| _ => error("No es un Record", nl)
			    val (expart, tyatr) = case List.filter (fn (x,t,z)=> x = s) lat of
					 [(_ , t, z)] => (z, t)
					| _ => error ("Atributo inexistente", nl)
			in 
                           {exp=fieldVar(expvar, expart), ty=tyatr}
			end 

		| trvar(SubscriptVar(v, e), nl) =
			let val {exp=expvar, ty=tyvar} = trvar (v, nl)
			    val ty' = case tyvar of
					TArray (t, u) => t
					| _ => error ("No es un arreglo", nl)
			    val {exp=expind, ty= tyind} = trexp e
			in
			    if tyind<>TInt then error("Indice no entero", nl)
			    else {exp =subscriptVar(expvar,expind), ty = ty'}
			end 
		
		 and transDecs (venv, tenv) (VarDec({name, escape, typ=NONE, init},nl)) =
			let val {exp=expinit, ty=tyinit} = transExp (venv, tenv) init
			    val _ = case tyinit of
					TNil => error ("La variable no puede tener el valor nil si no es un record", nl)
					| _ => ()
			    val varacc = allocLocal (topLevel()) (!escape) 
			    val nivel = getActualLev()
			    val venv' = tabRInserta (name, Var{ty=tyinit,readOnly = false, access=varacc, level=nivel}, (fromTab venv))
			    val {exp=expvar, ty=tyvar} = transExp (venv', tenv) (VarExp (SimpleVar (name), nl))
			in
			    (venv', tenv,[varDec(expvar, expinit)])
			end

		| transDecs (venv, tenv) (VarDec({name, escape, typ= SOME t, init}, nl)) =
			let val {exp=expinit, ty=tyinit} = transExp (venv, tenv) init 
			    val ty' = case tabBusca (t,tenv) of 
					SOME e => e
					|NONE => error("El tipo es inexistente", nl) 	
			    val _ = case (ty', tyinit) of
					(TRecord _, TNil) => ()
					|(TNil, TRecord _ ) => ()
					| _ => if ty'=tyinit then ()  else error("Los tipos no concuerdan", nl)
			    val varacc = allocLocal (topLevel()) (!escape) 
			    val nivel = getActualLev()
			    val venv' = tabRInserta (name, Var{ty = ty',readOnly=false, access=varacc, level= nivel}, (fromTab venv))
			
			    val {exp=expvar, ty=tyvar} = transExp (venv', tenv) (VarExp (SimpleVar (name), nl))
			in 
			    (venv', tenv,[varDec(expvar, expinit)])
			end 

		| transDecs (venv, tenv) (FunctionDec ldf)=
			let val venv' =tabNueva()
			    fun isnFun  ({name, params, result, body}, nl) = 
			        let val nombres = List.map (fn {name, escape, typ} => name) params
				    val repetidos = case dup nombres of 
						     false => true
						     |true => error ("Hay nombres de argumentos repetidos", nl)
				    val tret = case result of
						SOME t => (case tabBusca (t, tenv) of
								SOME e => e
								|NONE => error ("El resultado de la funcion no es un tipo", nl))
						|NONE => TUnit
				   
				    fun transparam {name, escape, typ}=
							case typ of 
							NameTy s => (case tabBusca(s, tenv) of
									SOME t => {name=name, ty=t}
									|NONE => error("No existe el tipo del argumento", nl))
							|_=> error("Falta declarar el tipo del argumento", nl)
				    val name' = case name of 
						"_tigermain"=> name
						| _ => name^"_"^Int.toString(nl)
	
				    val params'= List.map transparam params 
				    val ent = Func{level=topLevel(), label=name', formals=List.map #ty params' , result = tret, extern=false}				       val _ = case (tabBusca (name, venv')) of 
						SOME e => raise Fail ("Nombre de función repetido en el batch")
						| _ => ()
				    val _ = tabRInserta (name, ent, venv')
				in
				    ()
				end  
			        val _ = List.app  isnFun  ldf
				val lvenv = tabAList venv' 
				
			        val _ = List.map  (fn (a,b) => tabRInserta (a,b, venv)) lvenv 
		           
				fun enterParams (v, tv, l) nombre ldp ldd nl=
				    let val v' = fromTab v
					val name' = case tabBusca(nombre, v) of
						SOME(Func{result, label, ...}) => label
						| _ => error("El nombre de la función "^nombre^" no existe en el entorno", nl)
					val level' = preFunctionDec(topLevel(), name')
					val _ = pushLevel(level')
					val level'' = getActualLev()
					fun transparam {name, escape, typ} =
							(case typ of 
							NameTy s => (case tabBusca(s, tenv) of
									SOME t => tabRInserta(name,Var{ty=t, readOnly=false, access=allocArg (topLevel()) true ,level=level''}, v')
									|NONE => raise Fail "No existe el tipo del argumento")
							|_=> raise Fail "Falta declarar el tipo del argumento")
					val parametros = List.map transparam  ldp 
					val {exp=expbody, ty=tybody} = transExp (v', tv) ldd
					val _ = postFunctionDec()
					val exp' = functionDec(expbody, topLevel(), tybody=TUnit)
					val _ = popLevel() 
					val tret = (case (tabBusca(nombre, venv)) of 
								SOME (Func{level, label, formals, result, extern}) => if (igual tybody result) then () else error("El tipo del cuerpo de la función es distinto del tipo de retorno de la función", nl)
								| SOME e => error("El nombre no es una función", nl)
								| _ => error("No existe el nombre en la función", nl))
						
				    in 	
					(v, tv,[])
				    end
				val ltips= List.map (fn ({name, params, result, body}, nl) => (enterParams (venv, tenv, [])  name params body nl)) ldf
			in
				(venv, tenv,[])
			end

		|transDecs (venv, tenv) (TypeDec ldt) =
			let
			    val tenv' = tabNueva()

			    fun tins (n, v, t) =
				case tabBusca(n, t) of
					SOME _ => raise Fail ("El elemento "^n^" está duplicado.\n")
					|_ => tabInserta(n, v, t)

			    fun ingresa  n [] = []
				| ingresa n ({name=name, escape, typ}::xs) = (case typ of
									NameTy s => (name,TTipo (s, ref NONE), n):: ingresa (n+1) xs
									|_ => error ("Declaracion de campo de record incorrecta", n) )
			   
 			    fun trdec ({name, ty=NameTy s}, p) = tins(name, TTipo (s, ref NONE), tenv')
				| trdec ({name, ty=ArrayTy s}, p) = tins(name, TArray (TTipo (s, ref NONE), ref ()),tenv')
				| trdec ({name, ty = RecordTy l}, p) =  tins(name, TRecord (ingresa 0 l, ref ()), tenv')
			    
			    val _ = List.map trdec ldt      
			   
			    fun trl [] = []
				| trl ((id2, TTipo (id1,_ ))::xs) = (id1, id2)::(trl xs)
				| trl ((id2, TArray(TTipo (id1, _), _))::xs) = (id1, id2)::(trl xs) 
				| trl ((id2, TRecord (l, u))::xs)  =
					let 
					    val a = List.map (fn (s, t,i) => (id2, t)) l
					    val b = trl a 
					in  b@(trl xs)
					end
				| trl _ = raise Fail ("Error en los tipos del nuevo environment")
			   
			 
		           fun filtrar ls =
				List.filter (fn (id1, id2) => case tabBusca(id1, tenv') of 
								SOME (TRecord _) => false 
								|_ => true ) ls
				 
			   val ltenv = filtrar (trl (tabAList tenv'))
			      	
			   val lsorted = topsort ltenv  
					
			   fun busca s = case tabBusca(s, tenv') of 
						SOME e => e 
						| NONE => (case tabBusca(s, tenv) of 
								SOME t => t
								| _ => raise Fail (s^" tipo no definido"))    
			
			   fun reemplaza str b (TTipo (s, r)) = if str=s then b else (TTipo (s, r))
			   | reemplaza str b (TArray (r, u)) = TArray((reemplaza str b r), u)
			   | reemplaza str b (TRecord (ls, u)) =  
							let 
							val ls' = List.map (fn (s, t, i) => (s, reemplaza str b t, i)) ls
							in TRecord(ls', u)
							end
			   | reemplaza str b t = t
			
			
			   fun aplica t f =
					let val lt = tabAList t 
					    val _ = List.map (fn (a,b) => tabRInserta (a, f b,t)) lt 
					in () end
			   val _ = List.map (fn str => aplica tenv' (reemplaza str (busca str))) lsorted   
			    				 
			   fun rectypes (TTipo (s, r)) = (case (tabBusca(s, tenv')) of
									SOME e => (r:=(SOME e);TTipo(s, r))
									| _ => raise Fail ("Error interno con el TTipo "^s))
			   | rectypes (TArray (s, u)) = TArray (rectypes s, u)
			   | rectypes (TRecord (ls,u )) = 
							let
							    val nombres = List.map (fn (a,b,c) => a) ls
							    val _ = case dup nombres of
								    false => ()
								    | true => raise Fail ("Nombres de campos repetidos") 
							    val ls' = List.map (fn (s, t, i) => (s, rectypes t, i)) ls
							in
							   TRecord (ls', u)
							end
			   | rectypes a = a

                           val _ = aplica tenv' rectypes

			   val tenv'' = fromTab tenv
			   val ltenv' = tabAList tenv'
			   val _ = List.map  (fn (a,b) => tabRInserta (a,b, tenv'')) ltenv' 
		           
    
		in (venv, tenv'',[])
		end

	in trexp end

fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=NONE, body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val {exp,ty} = transExp(tab_vars, tab_tipos) main
	in	() end
end



