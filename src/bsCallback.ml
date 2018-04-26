type error  = exn Js.nullable
type 'a callback = error -> 'a -> unit [@bs]
(* A computation takes a callback and executes it
   either with an error or with a result of type 'a. *)
type 'a t = 'a callback -> unit

(* Computation that returns x as a value. *)
let return x cb =
  cb Js.Nullable.null x [@bs]

(* Computation that returns an error. *)
let fail exn cb =
  cb (Js.Nullable.return exn) (Obj.magic Js.Nullable.null) [@bs]

(* Catch exception. *)
let (||>) current catcher cb =
  let cb = fun [@bs] err ret ->
    match Js.toOption err with
      | Some exn -> catcher exn cb
      | None     -> cb err ret [@bs]
  in
  current cb

(* Pipe current's result into next. *)
let (>>) current next cb =
  let fn = fun [@bs] err ret ->
    match Js.toOption err with
      | Some exn -> fail exn cb
      | None     ->
         try
           next ret cb
         with exn -> fail exn cb
  in
  current fn

let (&>) current ensure cb =
  current (fun [@bs] err ret ->
    ensure (fun [@bs] _ _ ->
      cb err ret [@bs]))

let discard fn cb =
  fn (fun [@bs] err _ ->
    cb err () [@bs])

external setTimeout : (unit -> unit [@bs.uncurry]) -> float -> unit = "" [@@bs.val]

let itera ?(concurrency=1) fn a cb =
  let total = Array.length a in
  let executed = ref 0 in
  let rec process () =
    match Js.Array.pop a with
      | Some v ->
          fn v (fun [@bs] err () ->
            match Js.toOption err with
              | Some exn -> fail exn cb
              | None -> setTimeout process 0.)
      | None ->
          incr executed;
          if !executed = total then
            return () cb
  in
  for _ = 1 to concurrency do
    setTimeout process 0.
  done

let iter ?concurrency fn l =
  itera ?concurrency fn (Array.of_list l)

let fold_lefta ?concurrency fn a ba =
  let cur = ref a in
  let fn b =
    !cur >> fun a ->
      cur := fn a b;
      return ()
  in
  itera ?concurrency fn ba >> fun () ->
    !cur

let fold_left ?concurrency fn cur l =
  fold_lefta ?concurrency fn cur (Array.of_list l)

let iteri ?concurrency fn l =
  let l =
    List.mapi (fun idx el ->
      (idx,el)) l
  in
  let fn (idx,el) =
    fn idx el
  in
  iter ?concurrency fn l

let mapa ?concurrency fn a =
  let ret = [||] in
  let map v =
    fn v >> fun res ->
      ignore(Js.Array.push res ret);
      return ()
  in
  itera ?concurrency map a >> fun () ->
    return ret

let map ?concurrency fn l =
  mapa ?concurrency fn (Array.of_list l) >> fun ret ->
    return (Array.to_list ret)

let mapi ?concurrency fn l =
  let l =
    List.mapi (fun idx el ->
      (idx,el)) l
  in
  let fn (idx,el) = 
    fn idx el
  in
  map ?concurrency fn l

let execute ?(exceptionHandler=fun exn -> raise exn) t cb =
  t (fun [@bs] err ret ->
    match Js.toOption err with
      | None -> cb ret
      | Some exn -> exceptionHandler exn)

let finish ?exceptionHandler t =
  execute ?exceptionHandler t (fun _ -> ())

let from_promise p = fun cb ->
  let on_success ret =
    return ret cb;
    Js.Promise.resolve ret
  in
  let on_error err =
    fail (Obj.magic err) cb;
    p
  in
  ignore(Js.Promise.then_ on_success p |> Js.Promise.catch on_error)

let to_promise fn =
  Js.Promise.make (fun ~resolve ~reject ->
    fn (fun [@bs] err ret ->
      match Js.toOption err with
        | Some exn -> reject exn [@bs]
        | None -> resolve ret [@bs]))
