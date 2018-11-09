module type Async_t = sig
  (** Generic type for a computation. *)
  type 'a t

  (** A computation that returns a result of type 'a. *)
  val return : 'a -> 'a t

  (** A computation that returns an error. *)
  val fail : exn -> 'a t

  (** Combine two computations.
   * Operates on a constant call stack if [noStack] is true
   * with the drawback that no call trace will be returned
   * in case of error. Default: [false].
   * [noStack] is ignored and always [true] for [Promise] computations. *)
  val compose : ?noStack:bool -> 'a t -> ('a -> 'b t) -> 'b t
  val (>>)    : 'a t -> ('a -> 'b t) -> 'b t

  (** Catch errors raised during a computation. *)
  val catch  : ?noStack:bool -> 'a t -> (exn -> 'a t) -> 'a t
  val (||>)  : 'a t -> (exn -> 'a t) -> 'a t

  (** Pipe a result through a function. Equivalent to:
   * [computation >> fun v -> return (fn v)] *)
  val pipe : ?noStack:bool -> 'a t -> ('a -> 'b) -> 'b t
  val (>|) : 'a t -> ('a -> 'b) -> 'b t

  (** Execute a callback regardless of success or failure.
   * Errors raised by the [unit t] computation are discared. *)
  val ensure : ?noStack:bool -> 'a t -> (unit -> unit t) -> 'a t
  val (&>)   : 'a t -> (unit -> unit t) -> 'a t

  (** Execute a function regardless of success or failure.
   * Errors raised by the function are discared. *)
  val ensure_pipe : ?noStack:bool -> 'a t -> (unit -> unit) -> 'a t
  val (|&>) : 'a t -> (unit -> unit) -> 'a t

  (** Discard a computation's result. *)
  val discard : 'a t -> unit t

  (** Repeat a computation. Tail-recursive. *)
  val repeat : (unit -> bool t) -> (unit -> unit t) -> unit t

  (** Same as repeat but with negative condition. *)
  val repeat_unless : (unit -> bool t) -> (unit -> unit t) -> unit t

  (** Execute a computation conditionaly. *)
  val async_if : bool t -> (unit -> unit t) -> unit t

  (** Same as async_if but with negative condition. *)
  val async_unless : bool t -> (unit -> unit t) -> unit t

  (** In the following [concurrency] refers to the number
   * of concurrent executions. It is meant as in the node
   * model of concurrency, i.e. JS code is always non-concurrent
   * but e.g. HTTP Get calls can be queued via the event loop. *)

  (** Fold over a list or array of elements. Tail-recursive.
   * Result order will be shuffled when using concurrency > 1. *)
  val fold_lefta : ?concurrency:int -> ('a -> 'b -> 'a t) -> 'a t -> 'b array -> 'a t 
  val fold_left  : ?concurrency:int -> ('a -> 'b -> 'a t) -> 'a t -> 'b list -> 'a t
  val fold_lefti : ?concurrency:int -> ('a -> int -> 'b -> 'a t) -> 'a t -> 'b list -> 'a t

  (** Iter over a list or array of computations. Tail-recursive.
   * Execution order will be shuffled when using concurrency > 1. *)
  val itera  : ?concurrency:int -> ('a -> unit t) -> 'a array -> unit t
  val iter   : ?concurrency:int -> ('a -> unit t) -> 'a list -> unit t
  val iteri  : ?concurrency:int -> (int -> 'a -> unit t) -> 'a list -> unit t

  (** Map results. Tail-recursive.
   * Result order will be shuffled when using concurrency > 1. *)
  val mapa : ?concurrency:int -> ('a -> 'b t) -> 'a array -> 'b array t
  val map  : ?concurrency:int -> ('a -> 'b t) -> 'a list -> 'b list t
  val mapi : ?concurrency:int -> (int -> 'a -> 'b t) -> 'a list -> 'b list t

  (** Execute a sequence of computations.
   * Execution order will be shuffled when using concurrency > 1. *)
  val seqa : ?concurrency:int -> unit t array -> unit t
  val seq  : ?concurrency:int -> unit t list -> unit t

  (** Force a list or array of computations to resolve with fixed concurrency. *)
  val resolvea : concurrency:int -> 'a t array -> 'a t array
  val resolve  : concurrency:int -> 'a t list -> 'a t list

  (** Execute a computation and pass its result to a callback.
   * Default [exceptionHandler] raises any received exception. *)
  val execute : ?exceptionHandler:(exn->unit) -> 'a t -> ('a -> unit) -> unit
  val finish  : ?exceptionHandler:(exn->unit) -> unit t -> unit
end

module Callback : sig
  (** callback type. Strict arity is enforced here. *)
  type error  = exn Js.Nullable.t
  type 'a callback = error -> 'a -> unit [@bs]

  (** A asynchronous computation takes a callback and executes it when it
     has finished, either with an error or with a result of type 'a. *)
  type 'a t = 'a callback -> unit

  include Async_t with type 'a t := 'a t
end

module type Wrapper_t = sig
  type 'a t
  val return : 'a -> 'a t
  val fail   : exn -> 'a t
  val to_callback   : 'a t -> 'a Callback.t
  val from_callback : 'a Callback.t -> 'a t
end

module Make(Wrapper:Wrapper_t) : Async_t with type 'a t = 'a Wrapper.t

(** Interface with Promise API. *)
val from_promise : 'a Js.Promise.t -> 'a Callback.t
val to_promise   : 'a Callback.t -> 'a Js.Promise.t

module Promise : Async_t with type 'a t = 'a Js.Promise.t
