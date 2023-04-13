signature tigerset =
sig
			type 'a tigerset
	
			val newSet:('a * 'a -> order) -> 'a tigerset 
			val singletonSet:('a * 'a -> order) -> 'a -> 'a tigerset  
			val fromListSet:('a list * ('a*'a->order)) -> 'a tigerset  
			val insertSet:('a tigerset  * 'a) -> unit 
			val addSet:('a tigerset  * 'a tigerset ) -> unit
			val memberSet:('a tigerset  * 'a) -> bool
			val unionSet:('a tigerset   * 'a tigerset ) -> 'a tigerset  
			val differenceSet:('a tigerset   * 'a tigerset ) -> unit
			val minusSet:('a tigerset  * 'a tigerset ) -> 'a tigerset  
			val deleteSet:('a tigerset  * 'a) -> unit 
			val subSet:('a tigerset  * 'a tigerset ) -> unit
			val interSet:('a tigerset  * 'a tigerset ) -> 'a tigerset 
			val addListSet:('a tigerset  * 'a list) -> unit
			val forAllSet:('a tigerset  * ('a -> unit)) -> unit
			val printSet:('a tigerset  * ('a -> string)) -> string
			val equalSet:('a tigerset  * 'a tigerset ) -> bool 
			val isEmptySet:('a tigerset ) -> bool
			val listItemsSet:('a tigerset ) -> 'a list 
			val cardinalSet:('a tigerset ) -> int 
			val setEmptySet:('a tigerset) -> unit 

end
	  

