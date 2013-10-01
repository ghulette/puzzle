open Util

type tile = {
  top : int; 
  right: int; 
  bottom: int; 
  left: int
}

let top t = t.top
let right t = t.right
let bottom t = t.bottom
let left t = t.left
let tile t r b l = {
  top = t; 
  right = r;
  bottom = b; 
  left = l
}

let rotate t = {
  top = t.left; 
  right = t.top; 
  bottom = t.right; 
  left = t.bottom
}

let tiles =
  [|
    tile 0 0 0 0;
    tile 0 0 0 0;
    tile (-1) 0 0 0;
    tile 0 0 0 0;
    tile 0 0 0 0;
    tile 1 0 0 0;
    tile 0 0 0 0;
  |]

(***********************************************************************

               +-----+
               |     |
               |  A  |
               |     |
   +-----+-----+-----+-----+
   |     |     |     |     |
   |  B  |  C  |  D  |  E  |
   |     |     |     |     |
   +-----+-----+-----+-----+
         |     |
         |  F  |
         |     |
         +-----+

***********************************************************************)

let connects s1 mt1 s2 mt2 =
  match mt1,mt2 with
  | None,_ -> true
  | _,None -> true
  | Some t1,Some t2 -> s1 t1 + s2 t2 = 0

let check a b c d e f = all
  [connects right b left c;
   connects right c left d;
   connects right d left e;
   connects right e left b;
   connects top a    top b;
   connects right a  top e;
   connects bottom a top d;
   connects left a   top c;
   connects top f    bottom c;
   connects right f  bottom d;
   connects bottom f bottom e;
   connects left f   bottom b]

let get_tile = function
  | None -> None
  | Some (n,r) ->
    let t = tiles.(n) in
    Some (repeat rotate t r)

let is_solution s =
  let get = fun k -> get_tile (lookup k s) in
  let a = get 'A' in
  let b = get 'B' in
  let c = get 'C' in
  let d = get 'D' in
  let e = get 'E' in
  let f = get 'F' in
  check a b c d e f

let print s =
  let f = fun (p,(t,r)) -> (Char.escaped p) ^ "=" ^ 
    (string_of_int t) ^ "/" ^ (string_of_int r) in
  let s = List.fold_left (fun a b -> a ^ " " ^ b) "" (List.map f s) in
  print_endline s

let rec solve tiles places current =
  match places with
  | [] -> if is_solution current then print current
  | p::ps -> let tts = pick_one tiles in
    List.iter begin fun (t,ts) ->
      solve ts ps ((p,(t,0))::current);
      solve ts ps ((p,(t,1))::current);
      solve ts ps ((p,(t,2))::current);
      solve ts ps ((p,(t,3))::current)
    end tts

