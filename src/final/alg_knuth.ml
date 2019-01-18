(* implementation de l'algorithme de Knuth *)
open Code;;
module Alg_knuth:
sig
	(** Nombre d ' algorithmes developpes *)
	val nombre_methodes : int
	(** Choisit un code a proposer
	* @param methode 
	*       1 pour l ' algorithme naif,
	*       0 pour l ' algorithme de KNUTH
	*       ... et ainsi de suite
	* @param essais la liste des codes deja proposes
	* @param possibles la liste des codes possibles
	* @return le prochain code a essayer
	*)
	val choix : int -> Code.t list -> Code.t list -> Code.t
	(** Filtre les codes possibles
	* @param methode 0 pour l ' algorithme naif,
	*                1 pour l ' algorithme de KNUTH
	*               ... et ainsi de suite
	* @param (code, rep) le code essaye et la reponse correspondante
	* @param la liste de courante de codes possibles
	* @return la nouvelle liste de codes possibles
	*)
	val filtre :int -> (Code.t * (int * int)) -> Code.t list -> Code.t list
	
	
	(**Lance les test de l'algo de Knuth
		*)
	val test : (int * int) -> int -> float

end = struct

	let nombre_methodes = 2;;

	open Code;;
	(** Filtre les codes possibles
	* @param methode 0 pour l ' algorithme naif,
	*                1 pour l ' algorithme de KNUTH
	*               ... et ainsi de suite
	* @param (code, rep) le code essaye et la reponse correspondante
	* @param la liste de courante de codes possibles
	* @return la nouvelle liste de codes possibles
	*)
	let rec filtre2 algo codeEssaye codePossible  = 
		if codePossible != [] && algo = 1 then
			let rep = reponse (List.hd codePossible) (fst codeEssaye) in
			if rep = (snd codeEssaye) then
				(List.hd codePossible) :: filtre2 algo codeEssaye (List.tl codePossible) 
			else
				filtre2 algo codeEssaye (List.tl codePossible)
		else 
			[];;

	let calculPoid element s =
		let liste = filtre2 1 element s in List.length liste;;
	
	(** incremente le score en base 4
	* @param le score
	* @return le nouveau score
	*)
	let ajoutScore score =
		if (snd score) = 4 then
			((fst score) + 1, 0 )
		else
			((fst score),(snd score) + 1);;

	let rec testToutScore element s = 
		if (fst(snd element)) < 4 then
			let score = ajoutScore (snd element) in 
			let element = ((fst element), score) in
			let poids = calculPoid element s in 
			max poids (testToutScore element s)
		else
				0;;

	let rec testToutCode score temp min s = 
		if temp < (List.length s) then
			let nouveaux = testToutScore ((List.nth s temp),score) s in
			if nouveaux < (snd min) then
				testToutCode score (temp + 1) (List.nth s temp,nouveaux) s
			else
				testToutCode score (temp + 1) min s
		else
			(fst min);;

	(** applique la fonction minMax sur tout les codes
	* @param le score du test 
	* @param la liste de tout les code encore possible
	* @return le code a tester
	*)
	let rec minMax score s =
		testToutCode score 0 ([], (List.length s)) s;;
	

	(** Choisit un code a proposer
	* @param methode 
	*       1 pour l ' algorithme naif,
	*       0 pour l ' algorithme de KNUTH
	*       ... et ainsi de suite
	* @param essais la liste des codes deja proposes
	* @param possibles la liste des codes possibles
	* @return le prochain code a essayer
	*)
		let choix algo codeDeja codePossible =
		if (List.length codePossible) = 1 then (*si 1 element dans la liste *)
			List.nth codePossible 0
		else if algo = 3 then
			["Vert";"Vert";"Vert";"Vert"]
		else if algo = 0 then (*minMax pour Knuth*)
			minMax (0,0) codePossible
		else if algo = 1 then (*Naif*)
			List.nth codePossible (Random.int (List.length codePossible))
		else 
			List.nth codePossible (Random.int (List.length codePossible));;

	let rec filtre_algo_naif code listCode =
		if listCode != [] && code != List.hd listCode then
			(List.hd listCode) :: filtre_algo_naif code (List.tl listCode)
		else if code = List.hd listCode then
			List.tl listCode
		else
			[];;

	(** Filtre les codes possibles
	* @param methode 0 pour l ' algorithme naif,
	*                1 pour l ' algorithme de KNUTH
	*               ... et ainsi de suite
	* @param (code, rep) le code essaye et la reponse correspondante
	* @param la liste de courante de codes possibles
	* @return la nouvelle liste de codes possibles
	*)
	let rec filtre algo codeEssaye codePossible  = 
		if codePossible != [] && (algo = 0 || algo = 2) then
			let rep = reponse (List.hd codePossible) (fst codeEssaye) in
			if rep = (snd codeEssaye) then
				(List.hd codePossible) :: filtre algo codeEssaye (List.tl codePossible) 
			else
				filtre algo codeEssaye (List.tl codePossible) 
		else if algo = 1 then
			filtre_algo_naif (fst codeEssaye) codePossible
		else 
			[];;
	let rec testAlgo numero listeCode codeSecret temp =
		let codeTest = choix numero [] listeCode in
		let listeCode = filtre 1 (codeTest,reponse codeTest codeSecret) listeCode in
		if (List.length listeCode) <= 1 then
			temp
		else
			testAlgo 1 listeCode codeSecret (temp + 1);;

	let rec codeRandome acc =
		if acc <= 3 then
			let possible = ["Rouge"; "Vert"; "Bleu"; "Orange"; "Noir"; "Blanc"] in
			let nombre = Random.int (List.length possible) in (List.nth possible nombre) :: codeRandome (acc + 1)
		else
			[];;

	let rec test essaie iterations =
		if iterations >= 0 then
			let ensemble = tous in let essaie = (((fst essaie) + (testAlgo 0 ensemble ["Noir";"Noir";"Noir";"Noir"] 0)),((snd essaie) + 1)) in test essaie (iterations -1)
		else
			(float_of_int (fst essaie)) /. (float_of_int (snd essaie)) +. 1.;;
end ;;
