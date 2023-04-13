signature tigercodegen =
sig
	
	val codegen:tigerframe.frame*tigertree.stm->tigerassem.instr list
	val impresor:tigerassem.instr->unit
	val generador:tigercanon.fragCanon list ->unit

end

