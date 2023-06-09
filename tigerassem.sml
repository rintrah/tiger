structure tigerassem :> tigerassem  = 
struct
	open tigertemp

	type reg = string
	type temp = tigertemp.temp
	type label = tigertemp.label

	datatype instr = OPER of {	assem: string,
													 		dst: temp list,
													 		src: temp list,
													  	jump: label list option}
									| LABEL of {assem:string,
				    									lab: label}
									| MOVE of {	assem:string,
				  										dst: temp,
				   										src: temp}
	fun format saytemp =
		let 
			fun speak(assem,dst,src,jump) =
					let val saylab = tigertemp.labelAstring
					fun 	f(#"`":: #"s" :: i::rest) =
									(explode(saytemp(List.nth(src, ord i - ord #"0"))) @ f rest)
							| f( #"`":: #"d":: i::rest) =
									(explode(saytemp(List.nth(dst, ord i - ord #"0"))) @ f rest)
							| f( #"`":: #"j":: i::rest) = 
									(explode(saylab(List.nth(jump, ord i - ord #"0"))) @ f rest)
							| f( #"`":: #"`"::rest) = #"`" :: f rest
							| f( #"`":: _ ::rest) = raise Fail "Formato de assembler incorrecto"
							| f(c :: rest) = (c :: f rest)
							| f nil = nil
					in implode (f(explode assem))
					end
					in fn OPER{assem, dst, src, jump=NONE} => "\t" ^ (speak(assem,dst,src,nil))
							| OPER{assem, dst, src, jump=SOME j} => "\t" ^ (speak(assem,dst,src,j))
							| LABEL{assem, ...}=> assem
							| MOVE{assem,dst,src} => "\t" ^ (speak(assem,[dst],[src],nil))
					end

end

