structure tigertemp :> tigertemp =
struct
	type label = string and temp =string
	local 
		val nt = ref 0
		val nl = ref 0
	in
		fun newtemp () =
			"T"^Int.toString(!nt)
			before nt := !nt + 1
		fun newlabel () =
			"L"^Int.toString(!nl)
			before nl := !nl + 1
	fun namedlabel s = s
	end
	val fp = "ebp"
	val sp = "esp"
	val rv = "eax"
	val ov = "edx"
	fun makeString s = s
	fun tempAstring s = s
	fun labelAstring s = s
	fun stringAtemp s = s
	fun stringAlabel s =s 
	fun tempeq(a,b) = String.compare(a,b)
	fun cmptemp(a,b) = tempAstring(a) = tempAstring(b)
end
