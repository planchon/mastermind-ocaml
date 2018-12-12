(** Algorithmes de recherche de code *)
module IA :
sig
        (** Nombre d ' algorithmes developpes *)
        val nombre_methodes : int

        (** Choisit un code a proposer
         * @param methode 0 pour l ' algorithme naif, 1 pour l ' algorithme de KNUTH
         * ... et ainsi de suite
         * @param essais la liste des codes deja proposes
         *
         * @param possibles la liste des codes possibles
         * @return le prochain code a essayer
         *)
        val choix : int -> Code.t list -> Code.t list -> Code.t
                                                                 
        (** Filtre les codes possibles
         * @param methode 0 pour l ' algorithme naif, 1 pour l ' algorithme de KNUTH
         * ... et ainsi de suite
         * @param (code, rep) le code essaye et la reponse correspondante
         * @param possibles la liste de courante de codes possibles
         *
         * @return la nouvelle liste de codes possibles
         *)
        val filtre : int -> (Code.t * (int * int)) -> Code.t list -> Code.t list
end = struct
        let nombre_methodes = 2;;
        
        let choix methode propose possible =
          if (methode = 0) then (
                  ALG_Naif.algo propose (filtre 0 propose possible);
          ) else (
                  ALG_Knuth.algo propose (filtre 1 propose possible);
          );;

        let filtre methode reponse possible =
          if (methode = 0) then (
                  ALG_Naif.filtre_ (snd reponse) possible (fst reponse);
          ) else (
                  ALG_Knuth.filtre (snd reponse) possible (fst reponse);
          );;
end;;
