(* implementation de l'algo naif *)
module ALG_Naif:
sig
        val algo : Code.t list -> Code.t list -> Code.t;;
        val filtre : (Code.t * (int * int) option) -> Code.t list -> Code.t list
end = struct
        (* on suppose que les codes possible ont ete trie avec IA.filtre avant *) 
        let algo propose possible = ["code"];;
        let filtre reponse possible =
          let final = [] in
          let rec trie tableau = 
            match tableau with
            | a::b -> (
                    (* on regarde si le code a est un code viable *)
                    
                    
                    (* on rappelle la fonction sur le reste des codes *)
                    trie b;
            )
            | _ -> final;
          in trie possible;;
end;;
