# bs-callback

This module provides a Monadic API for callback computations in BuckleScript applications, as described in [this article](https://medium.com/@romain.beauxis/scalable-and-serverless-media-processing-using-bucklescript-ocaml-and-aws-lambda-api-gateway-4efe39331f33).

## Installation

```
yarn add https://github.com/lidcore/bs-callback.git
```

## Usage

Add `bs-callback` to the `bs-dependencies` of `bsconfig.json`.

## Rationale

Monadic computations are handy to write code that handles asynchronous errors and results in a composable 
fashion without having to repeat the same patterns again. For instance, from the `examples/` section:
```
module Fs : sig
  type fd
  val fopen  : string -> fd BsCallback.t
  val unlink : string -> unit BsCallback.t
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
(* BsCallback type. Strict arity is enforced here. *)
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
```
