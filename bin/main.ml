open Lala

let () =
  let code = match Array.to_list Sys.argv with
  | [] | [_] -> In_channel.input_all In_channel.stdin
  | _ :: h :: _ -> In_channel.with_open_text h (fun t -> In_channel.input_all t)
  in
  print_endline (eval_code code)
