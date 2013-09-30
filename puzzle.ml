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

let rotate90deg t =
  {top = t.left; right = t.top; bottom = t.right; left = t.bottom}

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

let all = List.fold_left (&&) true

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
