let span_to_ms span =
  let parts = Core.Time.Span.to_parts span in
  match parts with
  | { sign = _; hr; min; sec; ms; us; ns } ->
      (Float.of_int ns /. 1000000.)
      +. (Float.of_int us /. 1000.)
      +. Float.of_int ms
      +. (Float.of_int sec *. 1000.)
      +. (Float.of_int min *. 60. *. 1000.)
      +. (Float.of_int hr *. 60. *. 60. *. 1000.)

let time_ms f =
  let start = Core.Time.now () in
  let res = f () in
  let elapsed = Core.Time.diff (Core.Time.now ()) start |> span_to_ms in
  (res, elapsed)
