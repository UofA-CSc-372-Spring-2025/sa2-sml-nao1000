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

    Added later: Spent probably another two hours on it
    messing around and trying to improve code/
    write tests. For example, I way overcomplicated
    the concat function using nested foldls when it
    only needs one.
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
(* firstVowel char list -> bool
 * Using pattern matching, the function checks
 * if the first character in the list is a vowel *)
fun firstVowel [] = false
  | firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel (_::_) = false

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
(* reverse 'a list -> 'a list 
 * This function takes a list of any type
 * and reverses the ordering of the list *)
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
(* minList int list -> int 
 * This function finds the smallest integer
 * in a list. Will raise an exception with
 * and empty list *)
fun minlist [] = raise Match
  | minlist (x :: xs) = List.foldl (fn (x, acc) => Int.min (acc,x)) x xs

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
(* zip 'a list * 'b list -> ('a * 'b) list 
 * This function takes two lists of any type
 * creating a new list of pairs of the elements
 * in the same locations of each list. An exception is
 * is raised if the lists are unequal length *)
exception Mismatch of string

fun zip ([], []) = []
  | zip (x :: xs, y :: ys) = (x, y) :: zip(xs, ys)
  | zip (_, _) = raise Mismatch "Lists are of different lengths"

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
(* concat 'a list list -> 'a list
 * This function takes a list of lists that contain
 * elements of some type and combines all of the inner
 * lists into one list. *)
fun concat (l : 'a list list) : 'a list = 
  List.foldl (fn (x, acc) => acc @ x) [] l

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

val () =
  Unit.checkExpectWith (Unit.listString Char.toString)
  "concat [[#'a'], [#'b', #'c', #'d'], [], [#'e', #'f']] should be [#'a', #'b', #'c', #'d', #'e', #'f']"
  (fn () => concat [[#"a"], [#"b", #"c", #"d"], [], [#"e", #"f"]])
  [#"a", #"b", #"c", #"d", #"e", #"f"]

val () =
  Unit.checkExpectWith (Unit.listString Char.toString)
  "concat [[], [#'x'], [#'y', #'z']] should be [#'x', #'y', #'z']"
  (fn () => concat [[], [#"x"], [#"y", #"z"]])
  [#"x", #"y", #"z"]

val epsilon = 1.0E~6 (* a tolerance value for real comparison *)

(* approxEqual real * real -> bool 
 * This function compares two reals as
 * reals are not equality types *)
fun approxEqual(x: real, y: real) =
  abs(x - y) < epsilon

(* approxEqualList real list * real list -> bool
 * This function compares all of the reals in the two
 * lists and checks of the list are equal *)
fun approxEqualList ([], []) = true
    | approxEqualList (x :: xs, y :: ys) = approxEqual(x,y) andalso approxEqualList(xs, ys)
    | approxEqualList (_, _) = false

val () =
    Unit.checkExpectWith Bool.toString
    "concat [[], [7.7, 8.8], [9.9]] should be [7.7, 8.8, 9.9]"
    (fn () => approxEqualList (concat [[], [7.7, 8.8], [9.9]], [7.7, 8.8, 9.9]))
    true

val () =
  Unit.checkExpectWith Bool.toString
  "concat [[1.1], [2.2, 3.3], [], [4.4, 5.5]] should be [1.1, 2.2, 3.3, 4.4, 5.5]"
  (fn () => approxEqualList (concat [[1.1], [2.2, 3.3], [], [4.4, 5.5]], [1.1, 2.2, 3.3, 4.4, 5.5]))
  true


(**** Problem G ****)
(* isDigit char -> bool 
 * This function uses pattern matching
 * to check if the character is a digit *)
fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _ = false

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
(* isAlpha char -> bool 
 * This function uses a characters ASCII value
 * to see if it is an alphabetical character *)
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
(* svgCircle int * int * int * string -> string 
 * This function creates a formatted SVG string with 
 * given x and y cords, a radius, and a fill color *)
fun svgCircle (cx, cy, r, fill) = 
  "<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" ^ Int.toString cy ^
  "\" r=\"" ^ Int.toString r ^ "\" fill=\"" ^ fill ^ "\" />"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"

(**** Problem J ****)
(* partition ('a -> bool) -> 'a list -> 'a list * 'a list 
 * This function takes a predicate function and a list of
 * any type. All elements passing the predicate are stored
 * in the left list of the pair and the ones that fail are 
 * stored in the right list of the pair. *)
fun partition p [] = ([],[])
  | partition p (x :: xs) =
    let
      val (l1, l2) = partition p xs
    in
      if p x then (x::l1, l2)
      else (l1, x::l2)
    end

val showIntList = Unit.listString Int.toString

val () =
  Unit.checkExpectWith (Unit.pairString showIntList showIntList)
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5])

val () =
  Unit.checkExpectWith (Unit.pairString showIntList showIntList)
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x + 2 < 100) [1, 2, 3, 4, 5])
  ([1, 2, 3, 4, 5], [])

val () =
  Unit.checkExpectWith (Unit.pairString showIntList showIntList)
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x + 2 > 100) [1, 2, 3, 4, 5])
  ([], [1, 2, 3, 4, 5])

val () =
  Unit.checkExpectWith (Unit.pairString showIntList showIntList)
  "partition (fn x => x mod 2 = 0) [] should return ([], [])"
  (fn () => partition (fn x => x + 2 > 100) [])
  ([], [])

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)