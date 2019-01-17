(** Module de definition d'un code dans le jeu de Mastermind*)
module Code:
sig
        (** Le type d'un pion *)
        type pion = string

        (** Le type d'un code *)
        type t = pion list
               
        (** Le nombre de pions par code*)
        val nombre_pion : int          

        (** Le nombre de couleur et symboles possibles*)
        val couleur_possibles : pion list

        (** Compare deux codes
         *  @param code1 premier code a comparer
         *  @param code2 second  code a comparer
         *  @return 0 si les deux codes sont identiques,
                      un entier positif si [code1] est strictement plus grand que [code2]
                      un entier negatif si [code1] est strictement plus petit que [code2]       
         *)
        val compare : t -> t -> int

        (** Conversion chaine de caracteres vers code (pour saisie) 
         *  @param string chaine de caractere saisie
         *  @return le code correspondant a la saisie si la conversion est possible
                      [None] si la conversion n'est pas possible
         *)
        val string_of_code : t -> string

        (** Conversion chaine de caracteres vers code (pour saisie)
         *  @param string chaine de caractere saisie
         *  @return le code correspondant a la saisie si la conversion est possible
                      [None] si la conversion n'est pas possible
         *)
        val code_of_string : string -> t

        (** La liste de tous les codes permis *)
        val tous : t list
          
        (** La liste de toutes les reponses possibles *)  
        val toutes_reponses : (int * int) list
          
        (** Calcule la reponse d'un code par rapport au code cache
         *  @param      code le code propose
         *  @param vrai_code le code cache
         *  @return un couple (nombre de pions bien places, nombre de pions mal places)
            [None] si la reponse ne peut etre calculee
         *)
        val reponse : t -> t -> (int * int)

        val convert : int -> int -> int -> int list -> int list

        val finirTableau : int -> int list -> int list
end = struct    
        type pion = string;;
        type t = pion list;;
        let nombre_pion = 4;;
        let couleur_possibles = ["Rouge"; "Vert"; "Bleu"; "Orange"; "Noir"; "Blanc"];;

        let compare code1 code2 =
          match (code1, code2) with
          | a::b, c::d -> (if a = c then compare b d else
                                   if a > c then 1 else -1)
          | _, _ -> 0;;

        let string_of_code code = List.fold_right (fun x y -> x ^ "_" ^ y) code "";;

        let code_of_string code = Str.split (Str.regexp "_") code;;

        let rec convert x b i tmp =
          if x <= 0 then tmp
          else convert (x / b) b (i + 1) tmp @ [x mod b];;

        let finirTableau b tab =
          let taille = List.length tab in
          let rec aux final i tab = 
            if i >= final then tab
            else aux final (i + 1) (tab @ [0])
          in aux b taille tab;;
          
        let tous =
          let n = List.length couleur_possibles and m = nombre_pion in
          let rec aux i acc =
            if i = -1 then acc else aux (i - 1) acc @ [(List.map (fun x -> (List.nth couleur_possibles x))(finirTableau m (convert i n 0 [])))];
          in aux (int_of_float ((float_of_int n) ** (float_of_int m)) - 1) [];;
        
        let toutes_reponses =
          let m = nombre_pion in
          let rec aux i acc =
            let array = finirTableau 2 (convert i m 0 []) in
            if i < 0 then (List.filter (fun (x,y) -> if x + y < m then true else false) acc) else aux (i - 1) (acc @ [(List.nth array 0, List.nth array 1)]);
          in aux (m * m) [];;

        let supprFromListe a liste =
          let liste2 = (List.partition (fun x -> x = a) liste) in
          (List.tl (fst liste2)) @ (snd liste2);;
        
        let trouverCouleur code2 secret2 =
          let rec aux code secret tmp = 
            match code with
            | a :: b -> if (List.mem a secret) then
                                aux b (supprFromListe a secret) (tmp + 1)
                        else
                                aux b secret tmp
            | _ -> tmp;
          in aux code2 secret2 0;;
        
        let reponse code secret =
          let listeTotale = List.combine code secret in
          let bienPlace = List.partition (fun (x,y) -> x = y) listeTotale in
          (List.length (fst bienPlace), trouverCouleur (fst (List.split (snd bienPlace)))  (snd (List.split (snd bienPlace))));;
end;;

