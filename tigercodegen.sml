structure tigercodegen :> tigercodegen =
struct 
			
			open tigertemp
			open tigertree
			open tigerassem
			open tigerframe
		
			fun codegen (frame, stm: tigertree.stm) :tigerassem.instr list = 
			let val ilist = ref (nil:tigerassem.instr list)
					fun emit x = ilist := x :: !ilist
					fun result(gen) = let val t = newtemp() in gen t; t end
					fun itoa i = (if i < 0 then "-" else "")^(Int.toString(Int.abs(i))) 
					fun relOp relop =
							(case relop of 
										EQ => "je"
									|	NE => "jne"
									|	LT => "jl"
									| GT => "jg"
									| LE => "jle"
									| GE => "jge"
									| ULT => "jb"
									| ULE => "jbe"
									| UGT => "ja"
									| UGE => "jae")
					fun scaleFact n = n=0 orelse n=1 orelse n=2 orelse n=3
					fun sF n = 
						case n of
 						0 => 1
						|1 => 2
						|2 => 4
						|3 => 8
						| _ => raise Fail "Conversión a factor incorrecta.\n"
					fun munchStm(SEQ(a, b)) = (munchStm a; munchStm b)
						| munchStm(tigertree.MOVE(TEMP e1, MEM(BINOP (PLUS, TEMP e2, CONST i)))) =
								emit(OPER {assem = "movl "^itoa(i)^"(%`s0), %`d0 #0\n", 
															src=[e2],
															dst=[e1], jump=NONE})		
						| munchStm(tigertree.MOVE(TEMP e1, BINOP (PLUS, TEMP e2, CONST i))) =
								let 
									val tmp = newtemp()
								in
								(emit(OPER{assem= "movl %`s0, %`d0 \n",
											src = [e2], dst = [tmp], jump=NONE});
								 emit(OPER {assem = "addl $"^itoa(i)^", %`d0 \n",
											src = [], dst = [tmp], jump=NONE});
								 emit(OPER {assem = "movl %`s0, %`d0 #0' \n", 
															src=[tmp],
															dst=[e1],jump=NONE}))		
								end
						| munchStm(tigertree.MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2)) =
								emit(OPER {assem = "movl %`s0, "^itoa(i)^"(%`s1) #1\n",
															src = [munchExp e2, munchExp e1], 
															dst = [], jump=NONE})
						| munchStm(tigertree.MOVE(MEM(e1), MEM(e2))) =
								let 
										val tmp = newtemp()
								in
										(emit(OPER {assem = "movl (%`s0), %`d0 #2\n",
																src = [munchExp e2], dst = [tmp], jump=NONE});
										emit(OPER {assem = "movl %`s0, (%`s1) #3\n",
																src = [tmp, munchExp e1], dst = [], jump=NONE})) 
										end 
						| munchStm(tigertree.MOVE (MEM(e1), CONST i)) =
								emit(OPER {assem = "movl $"^itoa(i)^", (%`s0) #4\n", 
														src = [munchExp e1],
														dst = [], jump=NONE})
						| munchStm(tigertree.MOVE (MEM(e1), e2)) =
								emit(OPER {assem = "movl %`s0, (%`s1) #5\n",
														src = [munchExp e2, munchExp e1], 
														dst = [], jump=NONE})
						| munchStm(tigertree.MOVE (TEMP r, CONST i)) = 
								emit(OPER {assem = "movl $"^itoa(i)^", %`d0 #6\n",
														src = [], dst = [r], jump=NONE})
						| munchStm(tigertree.MOVE (TEMP i, NAME l)) =
								emit(OPER {assem = "movl $"^labelAstring(l)^", %`d0 #7\n",
											src = [], dst =[i], jump = NONE})
						| munchStm(tigertree.MOVE (e1, TEMP i)) =
									emit(OPER {assem = "movl %`s0, %`d0 #8\n",
											src = [i], dst = [ munchExp e1], jump=NONE})
						| munchStm(tigertree.MOVE (TEMP i, e2)) =
								emit(OPER  {assem = "movl %`s0, %`d0 #9\n",
														src = [munchExp e2],
														dst = [i], jump=NONE})
						| munchStm(EXP(CALL(NAME n, args))) =
							(emit(OPER {assem = "call "^labelAstring(n)^" #10\n",
										src = munchArgs((List.length args),  rev args), dst = callersaves, jump=NONE});
							if(List.length args > 0) then 
									emit(OPER {assem = "addl $"^itoa(List.length args * wSz)^" ,%esp #11\n",
												src = [], dst = [], jump=NONE})
							else ())
						| munchStm(EXP(CALL(e, args))) =
							(emit(OPER {assem = "call %`s0 #12\n",
										src =munchExp e::munchArgs((List.length args),  rev args), dst = callersaves, jump=NONE});
							if(List.length args > 0) then 
									emit(OPER {assem = "addl $"^itoa(List.length args * wSz)^" ,%esp #13\n",
												src = [], dst = [], jump=NONE})
							else ())
						| munchStm(EXP e) = (munchExp e; ())
						| munchStm (JUMP (NAME e,l)) =
							emit(OPER {assem = "jmp "^labelAstring(e)^" #14\n",
										src = [], dst = [], jump=SOME l})
						| munchStm (CJUMP (relop, e1,	e2, lt, lf))=
							let val () = emit(OPER{assem = "cmpl %`s0, %`s1 #15\n",
													src = [munchExp e2, munchExp e1], dst = [], jump=NONE})
							in
  								emit(OPER{assem = relOp(relop)^" "^labelAstring(lt)^" #16\n",
												src = [], dst = [], jump=SOME [lt]})
							end 
						| munchStm(tigertree.LABEL  lab) =
								emit (LABEL {assem = labelAstring(lab)^": #17\n", lab=lab}) 
						| munchStm _ = raise Fail "¡MunchStm incompleto!"
					and saveCallerSaves() =
						let fun emitcdefs s =
								emit (OPER{assem = "pushl %`s0 #18\n",
											src = [s], dst = [], jump=NONE})
						in
								List.map emitcdefs (tigerframe.callersaves)
						end
					and restoreCallerSaves() =
						let fun emitcdefs s =
								emit (OPER{assem = "popl %`s0 #19\n",
											src = [s], dst = [], jump=NONE})
						in 
								List.app emitcdefs (rev tigerframe.callersaves)
					end
					and  munchArgs(_, []) = []
						| munchArgs(i, h::t) =
							let val (instr, e) =
									case h of 
									TEMP t => (OPER{assem="pushl %`s0 #20\n", src=[t], dst=[], jump=NONE}, "")
										(*	(OPER {assem = "movl %`s0, "^itoa(i * wSz)^"(%esp)\n",
														src = [t], dst = [], jump=NONE}, "") *)
									| CONST t => (OPER{assem="pushl $"^itoa(t)^" #21\n", src=[], dst=[], jump=NONE}, "") 
												(*(OPER {assem = "movl $"^itoa(t)^", "^itoa(i * wSz)^"(%esp)\n",
														src = [], dst = [], jump=NONE}, "")*)
									| _ => 	let val e = munchExp h
													in
														(OPER {assem = "pushl %`s0  #22\n",
														src =[e], dst = [], jump=NONE}, "") 
													end
							in
								(emit(instr);
								if e<> "" then (stringAtemp e)::munchArgs(i-1, t) else munchArgs(i-1,t))
							end
					and  munchExp(ESEQ(s, e)) = (munchStm s; munchExp e)
						| munchExp(MEM(BINOP(PLUS, e, CONST i))) =
								result(fn r =>emit(OPER
												{assem = "movl "^itoa(i)^"(%`s0), %`d0 #22'\n",
													dst = [r], src = [munchExp e], jump=NONE})) 
						| munchExp(MEM(e1)) =
								result(fn r => emit(OPER 
														{assem = "movl (%`s0), %`d0 #23\n",
															src = [munchExp e1], dst = [r], jump=NONE}))
						| munchExp(BINOP(PLUS, CONST i, CONST j)) =
								result (fn r => (emit(OPER 	
														{assem = "movl $"^itoa(i+j)^" ,%`d0 #24\n",
															src =[], dst =[r], jump=NONE})))
						| munchExp(BINOP(PLUS, e1, CONST i)) =
								result (fn r => (emit(MOVE
														{assem = "movl %`s0, %`d0 #25\n",
															src = munchExp e1, dst = r});
																	emit(OPER
														{assem = "addl $"^itoa(i)^" , %`d0 #26\n",
															src =[], dst =[r], jump=NONE})))
						| munchExp(BINOP(PLUS, e1, e2)) =
								result (fn r => (emit(OPER 
														{assem = "movl %`s0, %`d0 #27\n",
															src = [munchExp e1], dst=[r],jump = NONE});
														emit(OPER 
														{assem = "addl %`s0, %`d0 #28\n",
															src = [munchExp e2, r], dst=[r], jump=NONE})))
						| munchExp(BINOP(MINUS, e1, e2)) =
								result(fn r => (emit(OPER
														{assem = "movl %`s0, %`d0 #29\n",
															src = [munchExp e1], dst = [r], jump=NONE});
														emit(OPER
														{assem = "subl %`s0, %`d0 #30\n",
														src = [munchExp e2, r], dst = [r], jump=NONE}))) 
						| munchExp(BINOP(MUL, e1, e2)) = 
								result(fn r => (emit(MOVE 
														{assem = "movl %`s0, %`d0 #31\n",
														src = munchExp e2, dst = r});
														emit(OPER{assem="imul %`s0, %`d0 #32\n",
														src = [munchExp e1, r], dst = [r], jump=NONE}))) 
						| munchExp(BINOP(DIV, e1, e2)) = 
								result(fn r => (emit(MOVE 
														{assem = "movl %`s0, %`d0 #33\n",
														src = munchExp e1, dst = rv});
														emit(OPER
														{assem = "cdq #34\n",
														src = [rv,ov], dst = [rv,ov], jump=NONE});
														emit(OPER 
														{assem = "divl %`s0 #36\n", src = [munchExp e2, rv, ov], dst = [rv, ov], jump=NONE});
														emit (MOVE {assem = "movl %`s0, %`d0\n", src = rv, dst = r})))
						| munchExp(BINOP(AND, e1, e2)) = 
								result(fn r => (emit(MOVE
														{assem = "movl %`s0, %`d0 #37\n", src = munchExp e1, dst = r});
														emit(OPER 
													{assem = "andl %`s0, %`d0 #38\n", src = [munchExp e2, r], dst = [r], jump=NONE})))
						| munchExp(BINOP(OR, e1, e2)) = 
								result(fn r => (emit(MOVE 
														{assem = "movl %`s0, %`d0 #39\n", src = munchExp e1, dst = r});
														emit(OPER
														{assem = "orl %`s0, %`d0 #40\n", src = [munchExp e2, r], dst = [r], jump=NONE})))
						| munchExp(CONST i)=
								result (fn r => (emit(OPER
														{assem = "movl $"^itoa(i)^" , %`d0 #41\n",
															src = [], dst = [r], jump=NONE})))
						| munchExp(TEMP t) = t
						| munchExp(NAME l) = result (fn r => (emit(OPER 
																{assem = "movl $"^(labelAstring l)^" , %`d0 #42\n",
																	src = [], dst = [r], jump=NONE})))
						| munchExp _ = raise Fail "Error: munchExp incompleto"
						
			in munchStm stm;
				rev(!ilist)

	end

	
	fun impresor operacion = print (format tigertemp.tempAstring operacion)
	fun generador (l:tigercanon.fragCanon list) =
			let 		
				fun aux(tigercanon.CanonProc (cb, f)) =
							let
									val listaInstr = procEntryExit2(f, List.concat (List.map (fn x => codegen(f,x)) cb))
							in
								(tigertrans.nombreFrame f;
							 	(*List.app impresor listaInstr;
							  print("\nAhora empieza el assembler definitivo\n");*)
								List.app impresor (tigerreg.coloreo(f, listaInstr)))
							(*print "\n"*)
							end
							
				|aux(tigercanon.CanonString (l, s)) =  if ((labelAstring l) = ";;-------") then ()
																								else (if ((labelAstring l) = "") then () 
																												else print ((labelAstring l)^":"); print (s^"\n"))
			in 
				(List.map aux l;())
	end 


end

