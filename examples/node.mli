module Fs : sig
  type fd
  val fopen  : string -> fd BsAsync.Callback.t
  val unlink : string -> unit BsAsync.Callback.t
end

val unlink_if_fopen : string -> unit BsAsync.Callback.t
