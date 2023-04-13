signature tigerframe =
sig

type frame
type register = string
val rv : tigertemp.temp
val ov : tigertemp.temp
val fp : tigertemp.temp
val ebx : tigertemp.temp
val ecx : tigertemp.temp
val edi : tigertemp.temp
val esi : tigertemp.temp
datatype access = InFrame of int | InReg of tigertemp.temp
val fpPrev : int
val fpPrevLev : int
val newFrame : {name: string, formals: bool list} -> frame
val name : frame -> string
val string : tigertemp.label * string -> string
val formals : frame -> access list
val allocArg : frame -> bool -> access
val allocLocal : frame -> bool -> access
val sp : tigertemp.temp
val wSz : int
val log2WSz : int
val calldefs : tigertemp.temp list
val callersaves : tigertemp.temp list
val calleesaves : tigertemp.temp list
val exp : access -> tigertree.exp -> tigertree.exp
val externalCall : tigertemp.label * tigertree.exp list -> tigertree.exp
val procEntryExit1 : frame * tigertree.stm -> tigertree.stm
val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list
val procEntryExit3 : frame * tigerassem.instr list -> tigerassem.instr list
datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string

end
