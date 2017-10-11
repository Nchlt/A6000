open IrAst
open IrLiveness

(* Élimination des instructions mortes.
   Renvoie un couple [(f, p)] où [p] est le programme simplifié, et
   où le booléen [b] vaut [true] si au moins une instruction a été
   éliminée.
     [dce_step: IrAst.main -> bool * IrAst.main]
 *)
let dce_step p =

  (* Calcul des informations de vivacité *)
  let _, lv_out = mk_lv p in

  (* À partir d'une instruction et de son étiquette, répond [true]
     si l'instruction est vivante, et [false] sinon.
       [live_instr: IrAst.label * IrAst.instruction -> bool]

     Une instruction morte est une instruction affectant une valeur à
     un registre virtuel qui n'est pas vivant en sortie de cette instruction.
     Toutes les autres sont vivantes.
  *)
  let live_instr = function
    (* Value et Binop affecte des valeurs, on doit verifier si l'id de la
      valeur qu'on affecte est vivante, c'est à dire si l'id de la variable
      a une entrée dans lv_out *)
    | (_, Value(id, _)) | (_, Binop(id, _, _, _)) ->
      Hashtbl.mem lv_out id
    (* Les instructions Print Label Goto Comment et CondGoto n'affectent
    aucune valeur, dans tout les autres cas on renvoie donc true *)
    (* | Print(_) | Label(_) | Goto(_) | Comment(_) -> true *)
    | _ -> true
  in

  (* Filtre la liste pour ne garder que les instructions vivantes *)
  let filtered_code = List.filter live_instr p.code in
  (* Renvoie le booléen et le code simplifié *)
  List.length p.code <> List.length filtered_code, { p with code=filtered_code }


(* Élimination itérée *)
let rec dce p =
  (* Tant qu'on supprime des instructions, on ré-itère sur le code filtré *)
  let has_changed, filt_code = dce_step p in
  if has_changed
  then dce filt_code else
  p
