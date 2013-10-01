open Util

type tile = {
  rot : int;
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
  rot = 0; 
  top = t; 
  right = r;
  bottom = b; 
  left = l
}

let rotate t = {
  rot = (t.rot + 1) mod 4; 
  top = t.left; 
  right = t.top; 
  bottom = t.right; 
  left = t.bottom
}

let connects s1 mt1 s2 mt2 =
  match mt1,mt2 with
  | None,_ -> true
  | _,None -> true
  | Some t1,Some t2 -> s1 t1 + s2 t2 = 0
    

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

let check (a,b,c,d,e,f) = all
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

module TileId = struct
  type t = A | B | C | D | E | F
  let compare = compare
  let ids = [A;B;C;D;E;F]
end

module Solution = Map.Make (TileId)

let rec place t s = function
  | [] -> []
  | p::ps ->
    let ss = place t s ps in
    if Solution.mem p s then ss else (Solution.add p t s) :: ss 

