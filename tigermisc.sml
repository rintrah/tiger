structure tigermisc :> tigermisc =
struct

fun curry f m n =  f(m,n)

fun uncurry f(m,n) = f m n

fun flip (m,n) =(n, m)

fun merge([], ys)         = ys
    | merge(xs, [])       = xs
    | merge(x::xs, y::ys) =
	if (String.compare(x,y) = GREATER) orelse (String.compare(x,y)=EQUAL)  then x::merge(xs, y::ys)
		  else y::merge(x::xs, ys)

fun mergesort   []        = []
    | mergesort [x]       = [x]
    | mergesort xs        =
	let val k = length xs div 2
	in merge (mergesort (List.take(xs, k)),
		  mergesort (List.drop(xs, k)))
	end
fun quick []  = []
    | quick [x] = [x]
    | quick ((a,b)::bs) =
	let fun  partition(left, right, [])= 
            (quick left) @ ((a,b)::quick right)
            | partition (left, right, (x,y)::xs)=
		if (String.compare(x,a)=EQUAL)orelse(String.compare(x,a)=LESS) then partition ((x,y)::left, right, xs)	
		else partition (left, (x,y)::right, xs)
		in partition([], [], bs) 
	end
fun dup [] = false
    | dup [x] = false
    | dup (x::y::xs) = if String.compare(x,y) = EQUAL then true else false 
end 