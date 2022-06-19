fun only_capitals(strings) = 
    List.filter(fn s => Char.isUpper(String.sub(s, 0))) strings

fun longest_string1(strings) = 
    List.foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2) "" strings

fun longest_string2(strings) = 
    List.foldl (fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2) "" strings

fun longest_string_helper (f) = 
    foldl (fn (s1, s2) => if f(String.size(s1), String.size(s2)) then s1 else s2) ""

val longest_string3 = 
    longest_string_helper (fn (x, y) => x > y)
 
val longest_string4 = 
    longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized =
    longest_string1 o only_capitals

val rev_string = 
    String.implode o rev o String.explode

exception NoAnswer

fun first_answer a b = 
    case b of
        [] => raise NoAnswer
        |(b::xs) => case a(b) of
                NONE => first_answer a xs
                |SOME v => v

fun all_answers f lst =
    let
        fun additional (lst, acc) =
            case lst of
                [] => SOME acc
                |x::xs => case f(x) of
                    NONE => NONE
                    |SOME v => additional(xs, v @ acc)
    in
        additional(lst, [])
    end

datatype pattern = Wildcard | Variable of string | UnitP | ConstP of int | TupleP of pattern list | ConstructorP of string * pattern
datatype valu = Const of int | Unit | Tuple of valu list | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

fun count_wildcards(p) = 
    g (fn () => 1) (fn x => 0) p

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (s, p) = g (fn x => 0) (fn y => if y = s then 1 else 0) p

fun check_pat(p) = 
    let 
        fun str_list(p) = 
            case p of
                Variable x => [x] 
                |TupleP ps  => List.foldl (fn (r, i) => str_list(r) @ i) [] ps
                | _ => []

        fun same_exist x = List.exists(fn (y) => x = y ) 
        
        fun uniqueness lst =
            case lst of
                [] => true
                | x::xs =>   if (same_exist x xs)
                            then false
                            else uniqueness(xs)
    in
        uniqueness(str_list(p))
    end 
