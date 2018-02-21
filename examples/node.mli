module Fs : sig
  type fd
  val fopen  : string -> fd Callback.t
  val unlink : string -> unit Callback.t
end

val unlink_if_fopen : string -> unit Callback.t
