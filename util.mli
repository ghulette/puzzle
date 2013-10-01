val pick_one : 'a list -> ('a * 'a list) list
val all : bool list -> bool
val any : bool list -> bool
val lookup : 'a -> ('a * 'b) list -> 'b option
val find_some : 'a option list -> 'a option
val repeat : ('a -> 'a) -> 'a -> int -> 'a
