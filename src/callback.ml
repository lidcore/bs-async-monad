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
    match Js.Nullable.to_opt err with
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

let iter fn l =
  let next cur el =
    cur >> fun _ ->
      fn el
  in
  List.fold_left next (return ()) l 

let iteri fn l =
  let pos = ref (-1) in
  let fn el =
    incr pos;
    fn !pos el
  in
  iter fn l

let execute t cb =
  t (fun [@bs] err ret ->
    match Js.toOption err with
      | None -> cb ret
      | Some exn -> raise exn)

let finish t =
  execute t (fun _ -> ())

let from_promise p = fun cb ->
  let on_success ret =
    return ret cb;
    Js.Promise.resolve ()
  in
  let on_error err =
    fail (Obj.magic err) cb;
    p
  in
  let p =
    Js.Promise.catch on_error p
  in
  ignore(Js.Promise.then_ on_success p)

let to_promise fn =
  Js.Promise.make (fun ~resolve ~reject ->
    fn (fun [@bs] err ret ->
      match Js.toOption err with
        | Some exn -> reject exn [@bs]
        | None -> resolve ret [@bs]))