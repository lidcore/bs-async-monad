module Fs : sig
  type fd
  val fopen  : string -> fd BsCallback.t
  val unlink : string -> unit BsCallback.t
end

val unlink_if_fopen : string -> unit BsCallback.t
