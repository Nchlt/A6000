(* Transformation de la syntaxe abstraite non typée
   vers la syntaxe abstraite "goto". *)
module S = UntypedAst
module T = GotoAst

let destructure_main p =

  (* new_label: unit -> string *)
  (* Un appel [new_label()] crée une nouvelle étiquette qui peut être
     utilisée pour créer des sauts. *)
  let new_label =
    let cpt = ref 0 in
    fun () -> incr cpt; Printf.sprintf "_label_main_%i" !cpt
  in

  (* destructure_block: S.block -> T.block *)
  let rec destructure_block = function
    | []     -> []
    | i :: b -> destructure_instruction i @ (destructure_block b)

  (* destructure_instruction: S.instruction -> T.block *)
  and destructure_instruction : S.instruction -> T.block = function
    | Set(loc, e) -> [ T.Set(loc, e) ]
    | Print(e) -> [ T.Print(e)  ]
    | While(cond, b) ->
        let b_while = destructure_block b in
        let l_start = new_label() in
        let l_while = new_label() in
        let l_end = new_label() in
        [T.Label(l_start)]@
        [T.CondGoto(cond, l_while)]@
        [T.Goto(l_end)]@
        [T.Label(l_while)]@
        b_while@
        [T.Goto(l_start)]@
        [T.Label(l_end)]
        (*
        Meilleure version :
        let test_label = new_label() in
        let code_label = new_label() in
        [ T.Goto(test_label)];
        T.Label(code_label)]@
        (destructure_block b)@ *)
    (*

        autre manière en utilisant la négation de cond :

        l_while
        T.CondGoto(!cond, l_end)
        b
        T.Goto(l_while)
        l_end
    *)

    | If(cond,b_then,b_else) ->
      (* b_ blocks, l_ labels *)
      let b_then = destructure_block b_then in
      let b_else = destructure_block b_else in
      let l_then = new_label() in
      let l_end = new_label() in
      let cgt_if = T.CondGoto(cond,l_then) in
      [cgt_if]@b_else@[T.Goto(l_end)]@[T.Label(l_then)]
      @b_then@[T.Label(l_end)]

    | _        -> failwith "A completer UntypedtoGoto.ml l63"
  in

  { T.locals = p.S.locals; T.code = destructure_block p.S.code }
