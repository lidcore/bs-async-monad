(* Callback type. Strict arity is enforced here. *)
type error  = exn Js.Nullable.t
type 'a callback = error -> 'a -> unit [@bs]

(* A asynchronous computation takes a callback and executes it when it 
   has finished, either with an error or with a result of type 'a. *)
type 'a t = 'a callback -> unit

(* A computation that returns a result of type 'a. *)
val return : 'a -> 'a t

(* A computation that returns an error. *)
val fail : exn -> 'a t

(* Catch errors raised during a computation. *)
val (||>)  : 'a t -> (exn -> 'a t) -> 'a t

(* Combine two computations. *)
val (>>) : 'a t -> ('a -> 'b t) -> 'b t

(* Iter over a list of computations. *)
val iter   : ('a -> unit t) -> 'a list -> unit t
val iteri  : (int -> 'a -> unit t) -> 'a list -> unit t

(* Execute a computation and pass its result to a callback. Errors are
   thrown/raised. *)
val execute : 'a t -> ('a -> unit) -> unit
val finish  : unit t -> unit

(* Interface with Promise API. *)
val from_promise : 'a Js.Promise.t -> 'a t
val to_promise   : 'a t -> 'a Js.Promise.t
