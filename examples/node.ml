open BsCallback

module Fs = struct
  type fd
  (* BsCallback with the right arity: *)
  external fopen : string -> fd callback -> unit = "open" [@@bs.module "fs"]

  (* BsCallback with different arity: *)
  external unlink : string -> (exn Js.Nullable.t -> unit [@bs]) -> unit = "" [@@bs.module "fs"]

  let unlink path cb =
    unlink path (fun [@bs] exn ->
      match Js.toOption exn with
        | Some exn -> fail exn cb
        | None     -> return () cb)
end

(* Pipe two asynchronous computations: *)
let unlink_if_fopen path =
  (Fs.fopen path) >> fun _ ->
    Fs.unlink path
