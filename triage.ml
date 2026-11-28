(* Triaging a record: one openness per (box, job, failure_class) window.

   The 399-run poison loop produced 399 records and one bug; this module turns
   a finished run into "open one card" or "annotate the open one". *)

type openness = {
  box : string;
  job : string;
  failure_class : string;
  opened_at : float;
}

(* The record's own words, so an exit 1 and a timeout never share a card. *)
let failure_class (r : Spec.run) : string option =
  match r.Spec.verdict with
  | `Ok -> None
  | `Timeout -> Some "timeout"
  | `Missing -> Some "missing"
  | `Failed reason -> Some reason

type decision = [ `Ignore | `Annotate of string | `Open of string ]

let default_window = 3600.

let decide ?(window = default_window) ~(open_ : openness list) (r : Spec.run) :
    decision =
  match failure_class r with
  | None -> `Ignore
  | Some cls -> (
    match
      List.find_opt
        (fun o ->
          o.box = r.Spec.box && o.job = r.Spec.job && o.failure_class = cls
          && r.Spec.started -. o.opened_at < window)
        open_
    with
    | Some _ -> `Annotate cls
    | None -> `Open cls)
