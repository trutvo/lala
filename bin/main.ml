open Lala

let () =
  let code = Sys.argv.(1) in
  print_endline (eval_code code)
  

