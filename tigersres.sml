structure tigersres =
struct

open tigerabs
open tigertab
open tigertips
open tigertrans

datatype EnvEntry =
	 Var of {ty: Tipo, readOnly: bool, access: access, level: int}
	| Func of {level:tigertrans.level, label:string,
		formals: Tipo list, result: Tipo, extern: bool}

end
