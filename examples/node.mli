module Fs : sig
  type fd
  val fopen  : string -> fd BsAsyncMonad.Callback.t
  val unlink : string -> unit BsAsyncMonad.Callback.t
end

val unlink_if_fopen : string -> unit BsAsyncMonad.Callback.t
