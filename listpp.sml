open tigertips
open tigersres

fun printTab(s, t) =
	let fun help (TArray (e, _)) = 
					let fun aux (TUnit) = print "TUnit "
		   					 	|aux (TNil ) = print "TNil " 
 		    					|aux (TInt ) = print "TInt "
		    					|aux (TString ) = print "TString "
		   						|aux (TArray (e, _) ) = (print "TArray of ";aux e)
		    					|aux (TRecord (l,_)) = 
														(print "TRecord "; List.app (printTab o (fn (name, typ, pos) => (name, typ))) l)    
		    					|aux (TFunc (l, _) ) = (print "TFunc: "; List.app aux l)
		    					|aux (TTipo (s, _) ) = print ("TTipo "^s^" ")	
					in
	    			(print("TArray "); aux e)
	 				end
	    | help (TTipo (s,_)) = print("TTipo "^s^" ")
	    | help (TRecord (l,_)) =
	    	let 
	    			fun f (name, typ,_) = (print(name^": "); help typ)
	   	 	in
						(print "TRecord: {"; List.app f l; print "}")
	   		 end
	    | help (TFunc (l, _)) =
	    							(print "TFunc {"; List.app help l; print  "} ")
	    | help (TNil) = print ("TNil ")
	    | help (TInt) = print ("TInt ")
	    | help (TString) = print ("TString ")
	    | help (TUnit) = print ("TUnit ")
	    
	in 
	    print ( "("^s^":"); print " "; help t; print ") \n"
end

fun printEnv(s, t) =
	let fun aux (Var {ty=TUnit, readOnly, access, level}) = print ("Var "^"TUnit ")
	    |aux (Var {ty=TNil, readOnly, access, level}) = print ("Var "^"TNil ")
	    |aux (Var {ty=TInt, readOnly, access, level}) = print ("Var "^"TInt ")
	    |aux (Var {ty=TString, readOnly, access, level}) = print ("Var "^"TString ")
	    |aux (Var {ty=TArray (_, _), readOnly, access, level}) = print ("Var "^"TArray ")
	    |aux (Var {ty=TRecord (l, _), readOnly, access, level}) =
	    let fun f (name, typ, _) =
					let fun ayuda (TUnit) = print "TUnit "
		    					|ayuda (TNil ) = print "TNil " 
 		    					|ayuda (TInt ) = print "TInt "
		    					|ayuda (TString ) = print "TString "
		    					|ayuda (TArray (_, _) ) = print "TArray "
		    					|ayuda (TRecord (l,_)) = 
													(print "TRecord "; List.app (printTab o (fn (name, typ, pos) => (name, typ))) l)    
		    					|ayuda (TFunc (l, _) ) = (print "TFunc: "; List.app ayuda l)
		    					|ayuda (TTipo (s, _) ) = print ("TTipo "^s^" ")
	        in 
						(print name; ayuda typ)
					end
	    in
	    			(print "TRecord"; List.app f l)
	    end
	    |aux (Var {ty=TTipo (s,_), readOnly, access, level}) = print ("Var "^"TTipo "^s^" ")
	    |aux (Func {level, label, formals, result, extern}) = 
	    			let fun ayuda (TUnit) = print "TUnit "
		    						|ayuda (TNil ) = print "TNil " 
 		    						|ayuda (TInt ) = print "TInt "
		    						|ayuda (TString ) = print "TString "
		    						|ayuda (TArray (_, _) ) = print "TArray "
		    						|ayuda (TRecord (l,_)) = 
														(print "TRecord "; List.app (printTab o (fn (name, typ, pos) => (name, typ))) l)    
		    						|ayuda (TFunc (_, _) ) = print "TFunc "
		    						|ayuda (TTipo (s, _) ) = print ("TTipo "^s^" ")
	    			in 	    	
	      			(print "Func: "; List.app ayuda formals)
	    			end
			|aux _ = raise Fail "Error: El environment contiene un tipo desconocido"
	in
	    		print ("(\""^s^"\""); print " "; aux t; print ") \n"
end  


