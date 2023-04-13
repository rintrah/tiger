structure tigerset :> tigerset  =
struct 
		open Splayset

		type 'a tigerset = 'a set ref 
		
		fun newSet cmp = ref (empty cmp)
		
		fun singletonSet cmp elem  = ref (singleton cmp elem)
		
		fun fromListSet(xs, cmp) = 	let val ns = newSet(cmp) 
																in
												 			 		ns := addList(!ns, xs); 
																	ns 
																end 
		    
		fun insertSet(conj, elem) = conj := add(!conj, elem)

		fun addSet(conj1, conj2) = conj1 :=  (union(!conj1, !conj2))

		fun memberSet(conj, elem) =  member(!conj, elem)

		fun unionSet(conj1, conj2) = ref  (union(!conj1, !conj2))

		fun differenceSet(conj1, conj2 ) =  conj1 := difference(!conj1, !conj2)
		
		fun minusSet(conj1, conj2) = ref (difference(!conj1, !conj2))

		fun deleteSet(conj, elem) = conj:= delete(!conj, elem) 

		fun subSet(conj1, conj2) = conj1 :=  (difference(!conj1, !conj2))

		fun interSet(conj1, conj2) = ref (intersection(!conj1, !conj2))
	
		fun addListSet(conj, xs) = conj:=addList(!conj,xs) 

		fun forAllSet (conj, f) = app f (!conj)

		fun printSet (conj, f) =  let 
																	val l = List.map (fn n => (f n)^" ,") (listItems (!conj))
															in "{ "^(String.concat l)^" }"
															end 
	
		fun equalSet (conj1, conj2) = equal(!conj1, !conj2)
		
		fun isEmptySet conj = isEmpty(!conj)
		
		fun listItemsSet conj = listItems(!conj)
	
		fun cardinalSet conj = numItems(!conj)

		fun setEmptySet conj = differenceSet(conj, conj)

 

end
