signature tigerassem =
sig
	 
		type reg
		type temp
		type label
		datatype instr =OPER of {assem:string,
														dst:tigertree.temp list,
														src:tigertree.temp list,
														jump: tigertemp.label list option} 
										| LABEL of {assem:string,
																lab: tigertree.label}
										| MOVE of {assem:string,
															dst:tigertree.temp,
															src:tigertree.temp}
		val format:(tigertemp.temp->string)->instr->string

end
	

