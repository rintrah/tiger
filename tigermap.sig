signature tigermap =
sig
		type ('key, 'a) tigermap

		val newMap:('key * 'key -> order) -> ('key, 'a) tigermap 
		val insertMap:('key, 'a) tigermap  -> 'key -> 'a -> unit
		val findMap:('key, 'a) tigermap  -> 'key -> 'a -> 'a
		val peekMap:('key, 'a) tigermap  -> 'key -> 'a option
		val numItemsMap:('key, 'a) tigermap  -> int	
		val listItemsMap:('key, 'a) tigermap  -> ('key * 'a) list
		val appMap:('key * 'a -> unit) -> ('key, 'a) tigermap  -> unit
		val forAllMap:('key * 'a -> unit) -> ('key, 'a) tigermap  -> unit
		val printMap:('key -> string) -> ('a -> string) -> ('key, 'a) tigermap  -> unit
		val getMap:('key,  'a) tigermap  -> 'key -> 'a 
		val setEmptyMap:('key * 'key -> order) -> ('key, 'a) tigermap -> unit 

end
