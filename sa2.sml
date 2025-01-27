(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Nathan Oswald                      *)
(* Time spent on SA2:
    Honestly, maybe an hour or two? Most of the
    problems were pretty easy to figure out after
    reading all of the SML documents (that took about
    an hour or so to get through). 

    Learning foldl was a little tricky but I figured it out
    and partition was the hardest because I was going
    in the right direction, but visually, the recursion
    was difficult. Using Copilot helped me figure out
    the val (l1, l2) = parition p xs which was similar
    to what I was trying to do, but I didn't realize I
    could set it up that way.
*)

(* Collaborators and references:
   Copilot
   I did not work on this with anyone as I 
   was able to get through the problems pretty easily
   (I also wanted to learn SML on my own first to get
    a hang of the syntax on my own before working with others)

   I am sure I will have helped some friends at some 
   point for this is due!
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

val () =
    Unit.checkExpectWith Bool.toString "mynull [1] should be false"
    (fn () => mynull [1])
    false

(**** Problem B ****)

fun firstVowel [] = false
  | firstVowel (x::xs) = (x = #"a" orelse x = #"e"
                   orelse x = #"i" orelse x = #"o"
                   orelse x = #"u")

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'bck' should be false"
    (fn () => firstVowel [#"b",#"c",#"k"])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'uck' should be true"
    (fn () => firstVowel [#"u",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel [] should be false"
    (fn () => firstVowel [])
    false

(**** Problem C ****)

fun reverse (l : 'a list) : 'a list =
  List.foldl (fn (x, acc) => [x] @ acc) [] l

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [2] should be [2]"
  (fn () => reverse [2])
  [2]

val () =
  Unit.checkExpectWith (Unit.listString Char.toString) 
  "reverse [#'a',#'c',#'k'] should be [#'k',#'c',#'a']"
  (fn () => reverse [#"a",#"c",#"k"])
  [#"k",#"c",#"a"]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [] should be []"
  (fn () => reverse [] : int list)
  []

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

val () =
  Unit.checkExpectWith Int.toString
  "minlist [0,1,2,3,4,0] should be 0"
  (fn () => minlist [0,1,2,3,4,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [0,1,2,~3,4,0] should be ~3"
  (fn () => minlist [0,1,2,~3,4,0])
  ~3

val () =
  Unit.checkExpectWith Int.toString
  "minlist [0,0,0,0,0,0] should be 0"
  (fn () => minlist [0,0,0,0,0,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [0] should be 0"
  (fn () => minlist [0])
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

val () =
  Unit.checkExpectWith (Unit.listString (fn (x, y) => "(" ^ Int.toString x ^ "," ^ Int.toString y ^ ")"))
  "zip ([], []) should []"
  (fn () => zip ([],[]) : (int * int) list)
  []

(**** Problem F ****)

fun concat (l : 'a list list) : 'a list = 
  List.foldl (fn (x, acc) => acc @ (List.foldl (fn (x2, acc2)
                          => acc2 @ [x2]) [] x)) [] l 


(* Probably better with foldr???)
fun concat (l : 'a list list) : 'a list = 
  List.foldr (fn (x, acc) => (List.foldr (fn (x2, acc2) => x2 :: acc2) [] x) @ acc) [] l  *)

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1, 2, 3, 4, 5, 6]"
  (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[], [1], [], [2, 3], [], [4, 5, 6], []] should be [1, 2, 3, 4, 5, 6]"
  (fn () => concat [[], [1], [], [2, 3], [], [4, 5, 6], []])
  [1, 2, 3, 4, 5, 6]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[], []] should be []"
  (fn () => concat [[], []])
  []

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1, 2, 3]] should be [1, 2, 3]"
  (fn () => concat [[1, 2, 3]])
  [1, 2, 3]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1], [2], [3], [4]] should be [1, 2, 3, 4]"
  (fn () => concat [[1], [2], [3], [4]])
  [1, 2, 3, 4]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1, 2], [3, 4, 5], [6]] should be [1, 2, 3, 4, 5, 6]"
  (fn () => concat [[1, 2], [3, 4, 5], [6]])
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

val () = 
  Unit.checkExpectWith Bool.toString
  "isDigit #'0' should be true"
  (fn () => isDigit #"0")
  true

val () = 
  Unit.checkExpectWith Bool.toString
  "isDigit #'9' should be true"
  (fn () => isDigit #"9")
  true

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

val () = 
  Unit.checkExpectWith Bool.toString
  "isAlpha #'A' should be false"
  (fn () => isAlpha #"A")
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

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x + 2 < 100) [1, 2, 3, 4, 5])
  ([1, 2, 3, 4, 5], []);

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x + 2 > 100) [1, 2, 3, 4, 5])
  ([], [1, 2, 3, 4, 5]);

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)