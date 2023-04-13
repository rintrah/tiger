signature tigertemp =
sig
	type label
	type temp
	val fp:temp
	val rv:temp
	val sp:temp
	val ov:temp 
	val newtemp:unit->temp
	val newlabel:unit->label
	val namedlabel:string->label
	val tempAstring:temp->string
	val labelAstring:label->string
	val makeString:label->string
	val stringAlabel:string->label
	val stringAtemp:string->temp
	val tempeq:temp*temp->order
	val cmptemp:temp*temp->bool
end
