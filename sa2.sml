(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Nathan Oswald                      *)
(* Time spent on HW6:
*)

(* Collaborators and references:
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true


(**** Problem B ****)

fun firstVowel [] = false
  | firstVowel (x::xs) = (x = #"a" orelse x = #"e"
                   orelse x = #"i" orelse x = #"o"
                   orelse x = #"u")

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

(**** Problem C ****)

fun reverse (l : 'a list) : 'a list =
  List.foldl (fn (x, acc) => [x] @ acc) [] l

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

(**** Problem D ****)

fun minlist (l : int list) : int =
  case l of
      []    => raise Match
    | x::xs => List.foldl (fn (x, acc) => Int.min (acc,x)) x l

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

(**** Problem E ****)

exception Mismatch of string

fun zip ([], []) = []
  | zip (x :: xs, y :: ys) = (x, y) :: zip(xs, ys)
  | zip (_, _) = raise Mismatch "Lists are of different lengths"

(* fun zip ((l1, l2) : ('a list, 'b list)) : ('a * 'b) list = 
  List.foldl (fn (x, y, acc) => (x, y) :: acc) [] l1 l2 *)

val () =
  Unit.checkExpectWith (Unit.listString (fn (x, y) => "(" ^ Int.toString x ^ "," ^ Int.toString y ^ ")"))
  "zip ([1,2], [2,1]) should be [(1,2), (2,1)]"
  (fn () => zip ([1,2], [2,1]))
  [(1,2), (2,1)]

val () =
  Unit.checkExnWith (Unit.listString (fn (x, y) => "(" ^ Int.toString x ^ "," ^ Int.toString y ^ ")"))
  "zip ([1,2], [2,1,3]) should raise an exceptions"
  (fn () => zip ([1,2], [2,1,3]))

(**** Problem F ****)

fun concat (l : 'a list list) : 'a list = 
  List.foldl (fn (x, acc) => acc @ (List.foldl (fn (x2, acc2) => acc2 @ [x2]) [] x)) [] l 


(* Probably better with foldr???)
fun concat (l : 'a list list) : 'a list = 
  List.foldr (fn (x, acc) => (List.foldr (fn (x2, acc2) => x2 :: acc2) [] x) @ acc) [] l  *)

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1, 2, 3, 4, 5, 6]"
  (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6]

(**** Problem G ****)

fun isDigit c = (c >= #"0" andalso c <= #"9")

val () = 
  Unit.checkExpectWith Bool.toString
  "isDigit #'7' should be true"
  (fn () => isDigit #"7")
  true

val () = 
  Unit.checkExpectWith Bool.toString
  "isDigit #'a' should be false"
  (fn () => isDigit #"a")
  false


(**** Problem H ****)

fun isAlpha c = 
  let 
    val x = Char.ord c
  in
    if x >= (Char.ord #"a") andalso x <= (Char.ord #"z") then true
    else if x >= (Char.ord #"A") andalso x <= (Char.ord #"Z") then true
    else false
  end

val () = 
  Unit.checkExpectWith Bool.toString
  "isAlpha #'7' should be true"
  (fn () => isAlpha #"7")
  false

val () = 
  Unit.checkExpectWith Bool.toString
  "isAlpha #'a' should be false"
  (fn () => isAlpha #"a")
  true

(**** Problem I ****)

fun svgCircle (cx, cy, r, fill) = 
  "<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" ^ Int.toString cy ^
  "\" r=\"" ^ Int.toString r ^ "\" fill=\"" ^ fill ^ "\" />"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";

(**** Problem J ****)

fun partition p [] = ([],[])
  | partition p (x :: xs) =
    let
      val (l1, l2) = partition p xs
    in
      if p x then (x::l1, l2)
      else (l1, x::l2)
    end

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);


(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
