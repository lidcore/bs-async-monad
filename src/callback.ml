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

let rec fold_left fn cur l =
  match l with
    | [] -> cur
    | el::l ->
      fold_left fn (cur >> fun v ->
        fn v el) l

external setTimeout : (unit -> unit [@bs.uncurry]) -> float -> unit = "" [@@bs.val]

let itera fn a cb =
  let rec process () =
    match Js.Array.pop a with
      | Some v ->
          fn v (fun [@bs] err () ->
            match Js.toOption err with
              | Some exn -> fail exn cb
              | None -> setTimeout process 0.)
      | None -> return () cb
  in
  setTimeout process 0.

let iter fn l =
  itera fn (Array.of_list l)

let iteri fn l =
  let pos = ref (-1) in
  let fn el =
    incr pos;
    fn !pos el
  in
  iter fn l

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
