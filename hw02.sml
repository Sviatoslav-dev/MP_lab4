(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*a*)
fun all_except_option (str, s_list) = 
   case s_list of
      [] => NONE
      |s::s_list' => if same_string(str, s) then 
            SOME s_list'
         else  
            case all_except_option(str, s_list') of
               NONE => NONE
               |SOME ls => SOME (s::ls);

(*b*)
fun get_substitutions1(sll, str) =
   case sll of
      [] => []
      |sl::sll' => case all_except_option(str, sl) of
         NONE => get_substitutions1(sll', str)
         |SOME ls => ls @ get_substitutions1(sll', str);

(*c*)
fun get_substitutions2(sll, str) =
   let
      fun aux_f(sll, str, res) = 
         case sll of
            [] => res
            |sl::sll' => case all_except_option(str, sl) of
               NONE => aux_f(sll', str, res)
               |SOME ls => aux_f(sll', str, ls @ res)
   in
      aux_f(sll, str, [])
   end;

(*d*)
fun similar_names(sll, full_name) = 
   let
      val {first=frst, middle=mdl, last=lst} = full_name
      fun aux_f(subs) = 
         case subs of
            [] => []
            |sub::subs' => {first=sub, middle=mdl, last=lst}::(aux_f(subs'))
   in
      full_name::aux_f(get_substitutions1(sll, frst))
   end;


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(*a*)
fun card_color(c) = 
   case c of
      (s, r) => if s = Hearts orelse s = Diamonds then Red
               else Black;


(*b*)
fun card_value(c) = 
   case c of
      (s, r) => 
         case r of 
            Ace => 11
            |Num n => n
            |_ => 10

(*c*)
fun remove_card(cs, c, e) = 
   case cs of
      [] => []
      |h::cs' => if h = c then 
            cs'
         else  
            case remove_card(cs', c, e) of
               [] => raise e
               |ls => h::ls;

(*d*)
fun all_same_color(cs) = 
   let
      fun aux_f(cs, color) = 
         case cs of
            [] => true
            |h::cs' => aux_f(cs', color) andalso card_color(h) = color
   in
      case cs of
         [] => true
         |h::cs' => aux_f(cs', card_color(h))
   end;

(*e*)
fun sum_cards (cs) =
    let
        fun aux(cs, sum) =
            case cs of
               [] => sum
             | h::cs' => aux(cs', card_value(h) + sum)
    in
        aux(cs, 0)
    end

(*f*)
fun score(cs, goal) = 
   let
      val sum = sum_cards(cs);

      fun get_score () = 
         if sum > goal then
            3 * (sum - goal)
         else 
            goal - sum;
   in
      if all_same_color(cs) then get_score() div 2
      else get_score()
   end;


(*g*)
fun officiate (cs, moves, goal) =
   let 
      fun aux_f(cs, moves, player_cards) = 
         case moves of 
            [] => player_cards
            |m::moves' => case m of 
               Discard d => aux_f(cs, moves', remove_card(player_cards, d, IllegalMove))
               |Draw => 
                  case cs of
                     [] => player_cards
                     |c::cs' => 
                        if sum_cards(c::player_cards) > goal then c::player_cards
                        else aux_f(cs', moves', c::player_cards)
   in
      score(aux_f(cs, moves, []), goal)
   end;
