let rec pick xs = function
  | [] -> []
  | x::ys -> (x,List.rev xs @ ys) :: pick (x::xs) ys;;

let pick_one xs = 
  pick [] xs

let all = 
  List.fold_left (&&) true

let any = 
  List.fold_left (||) false
