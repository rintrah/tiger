(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int

val fp = tigertemp.fp				(* frame pointer *)
val sp = tigertemp.sp				(* stack pointer *)
val rv = tigertemp.rv				(* return value  *)
val ov = tigertemp.ov				(* overflow value (edx en el 386) *)
val ebx = tigertemp.stringAtemp "ebx"
val ecx = tigertemp.stringAtemp "ecx"
val edi = tigertemp.stringAtemp "edi"
val esi = tigertemp.stringAtemp "esi"
val wSz = 4					(* word size in bytes *)
val log2WSz = 2				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) *)
val fpPrevLev = 8			(* offset (bytes) *)
val argsInicial = 0			(* ordinal *)
val argsOffInicial = 3*wSz	(* bytes *)
val argsDelta = wSz			(* bytes *)
val argsInc = 1
val localsInicial = 0		(* ordinal *)
val localsOffInicial = 0	(* bytes *)
val localsDelta = ~wSz 		(* bytes *)
val localsInc = ~1
val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = []
val callersaves = [rv,ecx,ov]
val calleesaves = [esi,edi, ebx]

type frame = {
	name: string,
	formals: bool list,
	locals: bool list,
	actualArg: int ref,
	actualLocal: int ref
}
type register = string
datatype access = InFrame of level | InReg of tigertemp.temp
datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string
fun newFrame{name, formals} = {
	name=name,
	formals=formals,
	locals=[],
	actualArg=ref argsInicial,
	actualLocal=ref localsInicial
}
fun name(f: frame) = #name f
fun string(l, s) = tigertemp.makeString(l)^s^"\n"
fun formals({formals=f, ...}: frame) = 
	let	fun aux(n, []) = []
		| aux(n, h::t) = InFrame(n)::aux(n+argsDelta, t)
	in aux(argsInicial, f) end
fun allocArg (f: frame) b = 
	case b of
	true =>
		let	val ret = !(#actualArg f)*wSz+argsOffInicial
			val _ = #actualArg f := !(#actualArg f)+argsInc
		in	InFrame ret end
	| false => InReg(tigertemp.newtemp())
fun allocLocal (f: frame) b = 
	case b of
	true =>
		let	val ret = InFrame(!(#actualLocal f)*wSz+localsDelta)
			val _ = #actualLocal f:=(!(#actualLocal f)+localsInc)
		in	ret end
	| false => InReg(tigertemp.newtemp())
fun exp(InFrame k) e = MEM(BINOP(PLUS, TEMP(tigertemp.fp), CONST k))
| exp(InReg l) e = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)
fun itoa i = Int.toString(Int.abs(i*wSz))
fun procEntryExit1(f, e) = e
fun procEntryExit2(frame:frame, body) =
(*	[tigerassem.OPER{assem="", src=[], dst=calleesaves, jump=NONE}]@
*)	body
@ [tigerassem.OPER{assem="# Instruccion trucha\n", src=[ rv ] @ calleesaves, dst=[], jump=SOME[]}] 

fun procEntryExit3(f:frame, body) =
	([tigerassem.OPER{assem="pushl %ebp\n", dst=[], src=[], jump=NONE}]@
	[tigerassem.OPER{assem="movl %esp, %ebp\n", dst=[], src=[],jump=NONE}]@
	(if (!(#actualLocal f) <> 0) then [tigerassem.OPER{assem="subl $"^itoa(!(#actualLocal f))^", %esp\n",
		 dst=[], src=[], jump=NONE }] else [])@
	body@
	(if (!(#actualLocal f) <> 0) then [tigerassem.OPER{assem="addl $"^itoa(!(#actualLocal f))^", %esp\n", 
	dst=[], src=[], jump=NONE}] else [])@
	[tigerassem.OPER{assem="movl %ebp, %esp\n", dst=[], src=[], jump=NONE}]@
	[tigerassem.OPER{assem="popl %ebp\nret\n", dst=[], src=[],jump=NONE}] )

end
