open IrAst

(* Création du graphe de flot de contrôle, sous la forme d'une table associant
   à chaque étiquette d'un point de programme les étiquettes de ses successeurs.
     [mk_succ: IrAst.block -> (IrAst.label * IrAst.label) Hashtbl.t]

   Note à propos des tables [Hashtbl] de Caml :

   - un appel [Hashtbl.add tbl k v] ajoute à la table [tbl] une association
     entre la clé [k] et la valeur [v], sans effacer définitivement une
     éventuelle association entre [k] et une autre valeur [v']

   - un appel [Hashtbl.find tbl k] renvoie la dernière valeur associée à
     la clé [k] ; lève l'exception [Not_found] s'il n'y a pas de valeur
     associée à [k]

   - un appel [Hashtbl.find_all tbl k] renvoie la liste de toutes les valeurs
     associées à la clé [k] ; renvoie la liste vide s'il n'y a aucune valeur
     associée

   - un appel [Hashtbl.replace tbl k v] ajoute à la table [tbl] une association
     entre la clé [k] et la valeur [v], en effaçant une éventuelle association
     précédente
*)

let mk_succ code =
  (* Création avec une capacité arbitraire ; la table sera étendue au besoin *)
  let succ = Hashtbl.create 257 in

  (* Parcours du code du programme et remplissage à la volée de la table *)
  let rec mk_succ : IrAst.block -> unit = function
    (* Cas offert : instruction [Goto] *)
    | (lab, Goto(target_lab)) :: code ->
      (* Le seul successeur d'une instruction [Goto] est l'instruction désignée
	 par l'étiquette de saut. *)
      Hashtbl.add succ lab target_lab;
      (* Puis on itère. *)
      mk_succ code
    (* Pour les cas suivants (Value, Binop, Print, Label, Comment
     ) le label suivant est celui de l'instruction suivante :*)
    | (lab, Value(_, _)) :: (next_lab, _) :: code ->
      Hashtbl.add succ lab next_lab;
      mk_succ code
    | (lab, Binop(_, _, _, _)) :: (next_lab, _) :: code ->
      Hashtbl.add succ lab next_lab;
      mk_succ code
    | (lab, Print(_)) :: (next_lab, _) :: code ->
      Hashtbl.add succ lab next_lab;
      mk_succ code
    | (lab, Label(_)) :: (next_lab, _) :: code ->
      Hashtbl.add succ lab next_lab;
      mk_succ code
    | (lab, Comment(_)) :: (next_lab, _) :: code ->
      Hashtbl.add succ lab next_lab;
      mk_succ code
    (* Il reste CondGoto qui a deux successeurs. le label de l'instruction
    suivante ainsi que celle du saut conditionnel *)
    | (lab, CondGoto(_, target_lab)) :: (next_lab, _) :: code ->
      Hashtbl.add succ lab target_lab;
      Hashtbl.add succ lab next_lab;
      mk_succ code
    | _ -> failwith "IrLiveness l57"
  in
  mk_succ code;
  (* À la fin, on renvoie la table qu'on a remplie *)
  succ


(* Définition des ensembles d'identifiants
   - le type d'un ensemble d'identifiants est [VarSet.t]
   - la constante [VarSet.empty] désigne l'ensemble vide
   - un appel [VarSet.singleton id] renvoie l'ensemble contenant uniquement
     l'identifiant [id]
   - les fonctions [VarSet.union] et [VarSet.diff] prennent en paramètres deux
     ensembles, et renvoient respectivement leur union et leur différence
     ensemblistes.
*)
module VarSet = Set.Make(String)

(* Fonction principale, renvoie deux tables associant à chaque étiquette d'un
   point de programme l'ensemble des variables vivantes en entrée/en sortie.
     [mk_lv: IrAst.main ->
      ((IrAst.label, VarSet.t) Hashtbl.t) * ((IrAst.label, VarSet.t) Hashtbl.t)]
*)
let mk_lv p =

  (* Création des deux tables destinées à accumuler le résultat, et
     calcul du flot de contrôle. *)
  let code = p.code in
  let lv_in  = Hashtbl.create 257
  and lv_out = Hashtbl.create 257
  and succ = mk_succ code
  in

  (* Initialisation des tables [lv_in] et [lv_out],
     associe [VarSet.empty] à chaque point de programme. *)
  List.iter (fun (lab, _) ->
    Hashtbl.add lv_in  lab VarSet.empty;
    Hashtbl.add lv_out lab VarSet.empty
  ) code;

  (* Les fonctions [lv_gen] et [lv_kill] prennent en paramètre une instruction
     et indiquent respectivement l'ensemble des variables vivantes qu'elle
     crée ou tue.
       [lv_gen:  IrAst.instruction -> VarSet.t]
       [lv_kill: IrAst.instruction -> VarSet.t]
  *)
  let rec lv_gen : IrAst.instruction -> VarSet.t = function
    | Print(v) ->
      (match v with
        | Identifier(id) -> VarSet.singleton id
        | _ -> VarSet.empty
      )
    | Value(_, v) ->
      (match v with
        | Identifier(id) -> VarSet.singleton id
        | _ -> VarSet.empty
      )
    | Binop(id, op, v1, v2) ->
      (match (v1, v2) with
        | (Identifier(id1), Identifier(id2)) ->
          let varset_1 = VarSet.singleton id1 in
          let varset_2 = VarSet.singleton id2 in
          VarSet.union varset_1 varset_2
        | (Identifier(id1), _) -> VarSet.singleton id1
        | (_, Identifier(id2)) -> VarSet.singleton id2
        | (_, _) -> VarSet.empty
      )
    | CondGoto(v, _) ->
      (match v with
        | Identifier(id) -> VarSet.singleton id
        | _ -> VarSet.empty
      )
    | Label(_) | Goto(_) | Comment(_) -> VarSet.empty
    | _        -> failwith "IrLiveness l135"

  and lv_kill : IrAst.instruction -> VarSet.t = function
    | Value(id, _) -> VarSet.singleton id
    | Binop(id, _, _, _) -> VarSet.singleton id
    | Print(_) | Label(_) | Goto(_) | CondGoto(_,_) | Comment(_) -> VarSet.empty
    | _        -> failwith "IrLiveness l141"
  in

  (* Booléen qu'on met à [true] lorsque les tables [lv_in] et [lv_out] sont
     encore en train de changer. Il est initialisé à [true] car à l'origine il
     y a bien du calcul à faire, mais il sera repassé à [false] avant chaque
     itération, pour n'être remis à [true] que si des changements sont
     observés. *)
  let change = ref true in

  (* Un appel [lv_step_instruction (lab, instr)] met à jour les entrées pour
     le point de programme [lab] (comportant l'instruction [instr]) dans les
     tables [lv_in] et [lv_out], en appliquant les équations de flot de données.
     Rappel :
        In[lab]  =  (Out[lab] \ Kill[instr]) ∪ Gen[instr]
       Out[lab]  =  ⋃ᵣ In[r]                                r ∈ Succ[lab]

     Cette fonction doit aussi faire passer le booléen [change] à [true] si
     les valeurs In[lab] et Out[lab] ont été modifiées.
  *)
  let lv_step_instruction (lab, instr) =
    (* Récupération de la liste des successeurs *)
    let succs = Hashtbl.find_all succ lab in (*List de (le_label, labelSuivant )*)
    (*
      On récupère le lv_out et lv_in "précédents"
      (Pas besoin de faire find all car les maj sont faites avec replace)
     *)
    let old_out_lab = Hashtbl.find lv_out lab in
    let old_in_lab = Hashtbl.find lv_in lab in
    (* Ce qui est généré et tué par l'instruction instr *)
    let kill_instr = lv_kill instr in
    let gen_instr = lv_gen instr in
    (* L'ensemble à ajouter au lv_in cf formule au dessus *)
    let in_add = VarSet.union (VarSet.diff old_out_lab kill_instr) gen_instr in
    (*
      On utilise la fonction suivante pour construire l'ensemble à ajouter
      à lv_out selon la formule du dessus. Au lieu de passer par une focntion
      récursive on aurait surement pu utiliser List.fold_left
    *)
    let rec mk_out acc l =
    match l with
    | [] -> acc
    | (_, lab) :: s ->
      let in_lab = Hashtbl.find lv_in lab in
      VarSet.union acc in_lab in
    (* Ce qu'il faut ajouter au lv_out : *)
    let add_out = mk_out VarSet.empty succs in
    (*
      Il reste maintenant à mettre à jour. La fonction suivante vérifie
      si on doit ajouter de nouveaux éléments (à lv_in/out). Si c'est le cas
      elle modifie change en conséquence.
    *)

    let update old_v add_v =
      let union = VarSet.union old_v add_v in
      let difference = VarSet.diff old_v union in
      if Set.Make.is_empty difference then (change := false;)
      else (
        change := true;
        Hashtbl.replace lv_out lab union;
        )
    in
    (* let update old_v add_v =
      let union = VarSet.union old_v add_v in
      let difference = VarSet.diff old_v union in
        (match difference with
        | VarSet.empty -> change := false;
        | _ ->
          change := true;
          Hashtbl.replace old_v lab union;)
     in *)

      (* On met à jour *)
      update old_in_lab in_add;
      update old_out_lab add_out;

    ()
  in

  (* Une passe complète : met à jour une fois chaque instruction *)
  let lv_step_main () =
    List.iter lv_step_instruction code
  in
  (* Répéter tant qu'il reste des changements *)
  while !change do
    change := false;
    lv_step_main ();
  done;
  (* Enfin, renvoyer les versions finales des tables *)
  lv_in, lv_out
