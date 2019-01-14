(* implementation de l'algorithme de Knuth *)
open Code;;
module ALG_Knuth:
sig        
        val filtre_ : (int * int) -> Code.t list -> Code.t -> Code.t list
        val algo : Code.t list -> Code.t list -> Code.t
end = struct
        let algo propose possible = ["code"];;
        let filtre_ reponse possible lastCode = [["code"]];;
end;;
