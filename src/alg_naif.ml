(* implementation de l'algo naif *)
open Code;;
open Random;;
module ALG_Naif:
sig        
        val filtre_ : (int * int) -> Code.t list -> Code.t -> Code.t list
        val algo : Code.t list -> Code.t list -> Code.t
end = struct

        let supprFromListe a liste =
          let liste2 = (List.partition (fun x -> x = a) liste) in
          (List.tl (fst liste2)) @ (snd liste2);;
        
        (* on suppose que les codes possible ont ete trie avec IA.filtre avant *) 
        let algo propose possible =
          let listePossible = 
            let rec supprime propose possible =
              match propose with
              | a::b -> supprime b (supprFromListe a possible)
              | _    -> possible;
            in supprime propose possible;
          in List.nth listePossible (Random.int (List.length listePossible));;
               
        (* on prends tous les codes possibles, et on ne retourne que ceux qui ont la meme reponse
         * que celui qu'on vient de jouer *)
        let filtre_ reponse possible lastCode =
          List.fold_left (fun t x -> if ((Code.reponse lastCode x) = reponse) then t @ [x] else t) [] possible;;
end;;
