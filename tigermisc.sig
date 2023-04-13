signature tigermisc =
sig

val curry:('a*'b->'c)->'a->'b->'c
val uncurry:('a->'b->'c)->('a*'b)->'c
val flip: ('a*'b)->('b*'a)
val merge: string list*string list ->string list
val mergesort: string list ->string list
val quick:(string *'b) list->(string *'b) list
val dup:string list->bool

end