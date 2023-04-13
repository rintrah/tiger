signature tigertrans = sig

type level
type access
type  frag=tigerframe.frag
val outermost : level
val newLevel : level * string * bool list -> level
val formals : level -> access list
val getActualLev : unit -> int
val allocArg : level -> bool -> access
val allocLocal : level -> bool -> access

type exp
val varDec:exp*exp->tigertree.stm
val imprime:exp->unit
val procEntryExit : {level: level, body: exp} -> unit 
val getResult : unit -> frag list
val unitExp : unit -> exp
val nilExp : unit -> exp
val intExp : int -> exp
val stringExp : string -> exp 
val simpleVar : access * int -> exp
(* val varDec : access -> exp *)
val fieldVar : exp * int -> exp 
val subscriptVar : exp * exp -> exp
val recordExp : exp  list -> exp (* Antes el tipo era de (exp * int) list *)
val callExp : string * bool * level * exp list * bool-> exp 
val letExp : tigertree.stm list * exp -> exp 
val breakExp : unit -> exp
val seqExp : exp list -> exp
val preWhileFor : unit -> unit
val postWhileFor : unit -> unit 
val whileExp : exp * exp -> exp
val forExp : exp * exp * exp * exp -> exp
val ifThenExp : exp * exp -> exp
val ifThenElseExp : exp * exp * exp -> exp 
val ifThenElseExpUnit :exp * exp * exp  -> exp 
val assignExp : exp * exp -> exp
val preFunctionDec : level * string -> level 
val functionDec : exp * level * bool -> exp
val postFunctionDec : unit -> unit 
val binOpIntExp : exp * tigerabs.oper * exp -> exp
val binOpIntExpEq : exp * tigerabs.oper * exp -> exp
val binOpStrExp : exp * tigerabs.oper * exp -> exp 
val arrayExp : exp * exp -> exp 
val nombreFrame:tigerframe.frame->unit
val Ir : frag list -> string

end
