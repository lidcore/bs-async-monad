type error  = exn Js.Nullable.t
type 'a callback = error -> 'a -> unit [@bs]
(* A computation takes a callback and executes it
   either with an error or with a result of type 'a. *)
type 'a t = 'a callback -> unit

val return : 'a -> 'a t
val fail   : exn -> 'a t
val (||>)  : 'a t -> (exn -> 'a t) -> 'a t
val (>>)   : 'a t -> ('a -> 'b t) -> 'b t
val iter   : ('a -> unit t) -> 'a list -> unit t
val iteri  : (int -> 'a -> unit t) -> 'a list -> unit t

val execute : 'a t -> ('a -> unit) -> unit
val finish  : unit t -> unit

val from_promise : 'a Js.Promise.t -> 'a t
val to_promise   : 'a t -> 'a Js.Promise.t
