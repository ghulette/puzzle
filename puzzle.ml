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

(*
let solution a b c d e f =
  match_tb d a &&
  match_lr d e &&
  match_lr c d &&
  match_lr b c &&
  match_tb _
*)
