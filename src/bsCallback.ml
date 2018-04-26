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

external setTimeout : (unit -> unit [@bs.uncurry]) -> float -> unit = "" [@@bs.val]

(* Pipe current's result into next. *)
let compose ?(noStack=false) current next cb =
  let fn = fun [@bs] err ret ->
    match Js.toOption err with
      | Some exn -> fail exn cb
      | None     ->
         let next = fun () ->
           try
             next ret cb
           with exn -> fail exn cb
         in
         if noStack then
           setTimeout next 0.
         else
           next ()
  in
  current fn

let (>>) = fun a b ->
  compose a b

(* Catch exception. *)
let catch ?(noStack=false) current catcher cb =
  let on_next next =
    if noStack then
      setTimeout next 0.
    else
      next ()
  in
  let cb = fun [@bs] err ret ->
    match Js.toOption err with
      | Some exn ->
          on_next (fun () ->
            catcher exn cb)
      | None     ->
          on_next (fun () ->
            cb err ret [@bs])
  in
  current cb

let (||>) = fun a b ->
  catch a b

let ensure ?(noStack=false) current ensure cb =
  let on_next next =
    if noStack then
      setTimeout next 0.
    else
      next ()
  in
  current (fun [@bs] err ret ->
    on_next (fun () ->
      ensure (fun [@bs] _ _ ->
        cb err ret [@bs])))

let (&>) = fun a b ->
  ensure a b

let discard fn cb =
  fn (fun [@bs] err _ ->
    cb err () [@bs])

let itera ?(concurrency=1) fn a cb =
  let total = Array.length a in
  let executed = ref 0 in
  let failed = ref false in
  let rec process () =
    let on_done () =
      incr executed;
      if not !failed && !executed = total then
        return () cb
      else
        setTimeout process 0.
    in
    match Js.Array.pop a with
      | Some v ->
          fn v (fun [@bs] err () ->
            match Js.toOption err with
              | Some exn ->
                  if not !failed then
                    fail exn cb;
                  failed := true
              | None -> on_done ())
      | None -> ()
  in
  for _ = 1 to min total concurrency do
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
