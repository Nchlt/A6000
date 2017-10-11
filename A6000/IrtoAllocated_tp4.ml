module S = IrAst
module T = AllocatedAst

(* Allocation *)
let allocate_main reg_flag p =
  let current_offset = ref 0 in
  let current_flag = ref (int_of_string reg_flag) in
  let tbl =
    if (!current_flag > 1 && !current_flag < 10)
    then

       (S.Symb_Tbl.mapi (fun id (info: S.identifier_info) ->
	match info with
	  | FormalX -> T.Reg (string_of_int !current_flag)
    | Local -> current_flag := !current_flag + 1 ;
      T.Reg (string_of_int !current_flag)
	  | _       -> failwith "A completer IrtiAllocated.ml l16"
      ) p.S.locals )
    else (
      (* Le reste sur la pile*)
      S.Symb_Tbl.mapi (fun id (info: S.identifier_info) ->
	match info with
	  | FormalX -> T.Stack 0
    | Local ->
      current_offset := !current_offset - 4;
      T.Stack !current_offset

	  | _       -> failwith "A completer IrtiAllocated.ml l27"
      ) p.S.locals )
  in

  { T.locals = tbl; T.offset = !current_offset; T.code = p.S.code }
