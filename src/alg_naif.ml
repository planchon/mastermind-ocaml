(* implementation de l'algo naif *)
module ALG_Naif:
sig
        val filtre_ : (int * int) -> Code.t list -> Code.t -> Code.t list
        val algo : Code.t list -> Code.t list -> Code.t
end = struct

        let bienPlace nombre lastCode testCode =
          let listeCombine = List.combine lastCode testCode in
          (nombre = (List.fold_right (fun x y -> x + y) (List.map (fun (x,y) -> if (x = y) then 1 else 0) listeCombine) 0));;

        let supprFromListe a liste =
          let liste2 = (List.partition (fun x -> x = a) liste) in
          (List.tl (fst liste2)) @ (snd liste2);;

        let malPlace nombre lastCode testCode =
          let listeCombine = List.combine lastCode testCode in
          (nombre = (List.fold_right (fun x y -> x + y) (List.map (fun (x,y) -> if ((x <> y) && (List.mem y lastCode)) then 1 else 0) listeCombine) 0));;
        
        (* on suppose que les codes possible ont ete trie avec IA.filtre avant *) 
        let algo propose possible = ["code"];;
        
        let filtre_ reponse possible lastCode =
          (* bp -> bien place, mp -> mal place*)
          let bp = (fst reponse) in
          let mp = (snd reponse) in
          List.filter (fun code2 -> malPlace mp lastCode code2) (List.filter (fun code -> bienPlace bp lastCode code) possible);;
        
end;;
