# bs-async-monad

This module provides a Monadic API for callback computations in BuckleScript applications, as described in [this article](https://medium.com/@romain.beauxis/scalable-and-serverless-media-processing-using-bucklescript-ocaml-and-aws-lambda-api-gateway-4efe39331f33).

It provides a unified API for dealing with both callback-based asychronous computations as well as promises. Whenever possible, the code is optimized for javascript.

## Installation

```
npm install [--save] bs-async-monad
```

Then add it to your `bsconfig.json`:

```
  ...
  "bs-dependencies" : [
    "bs-async-monad",
    ...
    ],
  ...
```

## Rationale

Monadic computations are handy to write code that handles asynchronous errors and results in a composable 
fashion without having to repeat the same patterns again. For instance, from the `examples/` section:
```
open BsAsyncMonad.Callback

module Fs : sig
  type fd
  val fopen  : string -> fd t
  val unlink : string -> unit t
end

(* Pipe two asynchronous computations: *)
let unlink_if_fopen path =
  (Fs.fopen path) >> fun _ ->
    Fs.unlink path
```

Here, errors raised during the first call are immediately passed to the final callback without 
having to repeatedly write that pattern over and over.

This is similar to the `Promise` API but with less overhead and a focus on composability.

The API is as follows:
```
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
```
