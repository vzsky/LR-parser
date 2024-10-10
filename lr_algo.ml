let rec unique lst =
  match lst with
  | [] -> []
  | x :: xs -> if List.mem x xs then unique xs else x :: unique xs
;;

let rec iota start stop =
  if start >= stop then []
  else start :: iota (start + 1) stop
;;

let str_to_list s =
  List.init (String.length s) (fun i -> s.[i])
;;

let str_of_list ls = 
  String.init (List.length ls) (List.nth ls)
;;

type production_rule = char * string;;
type item = char * string * int;;
type state = item list;;
type stack = state list;;

let stack_push (stack: stack) (x: state) :stack = List.cons x stack;; 
let rec stack_pop (stack: stack) (n: int) :stack = if n == 0 then stack else stack_pop (List.tl stack) (n-1);;
let stack_top (stack: stack) :state = List.hd stack;;

let augmented_grammar g = List.append g [('#', "S")];;

let is_non_terminal c = ('A' <= c && c <= 'Z') || c == '#';;
let is_terminal c = ('a' <= c && c <= 'z') || c == '$' ;;

let non_terminal grammars = 
  List.map (fun (e, ps) -> 
    String.fold_left (fun acc c -> if is_non_terminal c then c :: acc else acc) [e] ps
  ) grammars |> List.concat |> unique
;;

let terminal grammars = 
  List.map (fun (e, ps) -> 
    String.fold_left (fun acc c -> if is_terminal c then c :: acc else acc) [e] ps
  ) grammars |> List.concat |> unique
;;

let make_item grammar dot = let (e, ps) = grammar in (e, ps, dot) ;;

let items grammar = 
  let (_, ps) = grammar in
  List.map (make_item grammar) (iota 0 (String.length ps + 1))
;;

let show_item item = 
  let (e, ps, ind) = item in 
  let buf = Buffer.create 0 in 
  let addchar = Buffer.add_char buf in 
  let () = addchar e in 
  let () = (Buffer.add_string buf) " -> " in 
  let () = String.iteri ( fun i x -> if i == ind then addchar '.'; addchar x) (ps) in 
  let () = if ind == String.length ps then addchar '.' in
  Buffer.contents buf
;;

let rec first grammars nonterm = 
  List.map (fun g -> 
    let (e, ps) = g in 
    let f = ps.[0] in 
    if e == nonterm then 
      if is_terminal f then [f] 
      else first grammars f 
    else []
  ) grammars |> List.concat |> unique
;;

let rec follow grammars nonterm = 
  List.map (fun g -> 
    let (e, ps) = g in 
      str_to_list ps |> List.mapi (fun i p ->
      let next = try ps.[i+1] with Invalid_argument _ -> '$' in 
      if p == nonterm then 
        if is_non_terminal next then first grammars next
        else [next]
      else []
    ) |> List.concat
  ) grammars |> List.concat |> unique
;;

let rec closure grammars items = 
  List.map (fun item -> 
    let (e, ps, dot) = item in 
    try let next = ps.[dot] in 
      List.map (fun (e, ps) -> 
        if e == next then closure grammars [make_item (e, ps) 0]
        else []
      ) grammars |> List.concat
    with Invalid_argument _ -> []
  ) items |> List.concat |> (List.append items) |> unique
;;

let rec goto grammars items next_tok = 
  List.map (fun item -> 
    let (e, ps, dot) = item in 
    try let expected = ps.[dot] in 
      if expected == next_tok then [make_item (e, ps) (dot+1)] 
      else []
    with Invalid_argument _ -> []
  ) items |> List.concat |> unique |> (closure grammars)
;;

let reduce grammars stack rule = 
  let (e, ps) = rule in 
  let stack = stack_pop stack (String.length ps) in 
  stack_push stack (goto grammars (stack_top stack) e)
;;

type action = Shift | Reduce | Accept ;;
type action_entry = action * (char -> stack -> stack)

let action grammars items :action_entry = 
  if (List.exists (fun g -> g = ('#', "S", 1)) items) then (Accept, fun next_tok stack -> stack) else
  let reduce_rule = List.find_map (fun (e, ps, dot) -> if String.length ps == dot then 
    Some (Reduce, fun next_tok stack -> reduce grammars stack (e, ps)) 
  else None) items in

  match reduce_rule with 
  | Some r -> r 
  | None   -> (Shift, fun next_tok stack -> stack_push stack (goto grammars items next_tok)) 
;;

let lr0_parser grammars string = 
  let g = grammars |> augmented_grammar in 
  let now = closure g [('#', "S", 0)] in 
  let stack = [now] in  
  let tokens = str_to_list string in

  let rec parse stack tokens =
    match action g (stack_top stack) with 
    | (Reduce, f) -> parse (f '?' stack) tokens
    | (Accept, _) -> "Accept, Remaining: " ^ (str_of_list tokens)
    | (Shift, f) -> parse (f (List.nth tokens 0) stack) (List.tl tokens)
  in try parse stack tokens with 
  | Failure _ -> "Reject"
;; 

(*A few special characters: 
- $ is the EOF-ish character, a terminal 
- # is the augmented start, with the rule # -> S 
- S is a nonterminal that is considered the start non_terminal
*)

(* let grammars = [('S', "abA"); ('A', "cA"); ('A', "a")] *)
(* let () =  *)
(*   let parser = lr0_parser grammars in  *)
(*   let () = parser "abcccca" |> print_endline in  *)
(*   let () = parser "aba" |> print_endline in  *)
(*   let () = parser "abccccccca" |> print_endline in  *)
(*   let () = parser "abcaccca" |> print_endline in  *)
(*   let () = parser "abccbcca" |> print_endline in  *)
(*   () *)
(* ;; *)

(* let grammars = [('S', "EaE"); ('S', "E"); ('T', "FxT"); ('T', "F"); ('E', "T"); ('F', "n"); ('E', "oEc")];;  *)
(* let () = *)
(*   let result = non_terminal grammars in  *)
(*   let () = List.iter (Printf.printf "%c ") result in  *)
(*   let () = print_endline "" in  *)
(*   let item = items (List.nth grammars 0) |> List.nth in  *)
(*   let () = Printf.printf "%s" (show_item (item 1)) in *)
(*   let () = print_endline "" in *)
(*   let () = print_endline "First of S" in  *)
(*   let () = List.iter (Printf.printf "%c ") (first grammars 'S') in  *)
(*   let () = print_endline "" in  *)
(*   let () = print_endline "Follow of E" in  *)
(*   let () = List.iter (Printf.printf "%c ") (follow grammars 'E') in  *)
(*   let () = print_endline "" in  *)
(*   let () = print_endline ("Closure of " ^ show_item (item 0)) in  *)
(*   let () = List.iter (Printf.printf "%s, ") (List.map show_item (closure grammars [item 0])) in  *)
(*   let () = print_endline "" in  *)
(*   let () = print_endline ("Goto of " ^ show_item (item 0) ^ " with tok n") in  *)
(*   let () = List.iter (Printf.printf "%s, ") (List.map show_item (goto grammars (closure grammars [item 0]) 'n')) in  *)
(*   () *)
(* ;; *)
