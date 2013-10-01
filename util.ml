let rec pick xs = function
  | [] -> []
  | x::ys -> (x,List.rev xs @ ys) :: pick (x::xs) ys;;

let pick_one xs = 
  pick [] xs

let all = 
  List.fold_left (&&) true

let any = 
  List.fold_left (||) false

let rec lookup x = function
  | [] -> None
  | (k,v)::_ when x = k -> Some v
  | _::ys -> lookup x ys

let rec find_some = function
  | [] -> None
  | Some x::_ -> Some x
  | None::xs -> find_some xs

let rec repeat f x = function
  | 0 -> x
  | n -> repeat f (f x) (pred n);;
