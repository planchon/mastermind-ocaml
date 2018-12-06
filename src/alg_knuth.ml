(* implementation de l'algorithme de Knuth *)
module ALG_Knuth:
sig
        val algo : Code.t list -> Code.t list -> Code.t;;
        val filtre : (Code.t * (int * int) option) -> Code.t list -> Code.t list
end = struct
        let algo propose possible = ["code"];;
        let filtre reponse possible = [["code"]];;
end;;
