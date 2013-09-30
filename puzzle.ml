type tile = {top : int; right: int; bottom: int; left: int}

let rotate1 t =
  {top = t.left; right = t.top; bottom = t.right; left = t.bottom}

let rotate2 t =
  rotate1 (rotate1 t)

let rotate3 t =
  rotate1 (rotate2 t)

let match_tb t1 t2 =
  t1.top + t2.bottom = 0

let match_lr t1 t2 =
  t1.left + t2.right = 0

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
