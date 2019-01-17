(* implementation de l'algorithme de Knuth *)
open Code;;
module Alg_knuth:
sig
	(** Nombre d ' algorithmes developpes *)
	val nombre_methodes : int
	(** Choisit un code a proposer
	* @param methode 
	*       0 pour l ' algorithme naif,
	*       1 pour l ' algorithme de KNUTH
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
	(*filtre les codes possibles en fonction du code d'essaie et du score obtenue*)
	let rec filtre2 algo codeEssaye codePossible  = 
		if codePossible != [] && algo = 1 then
			let rep = reponse (List.hd codePossible) (fst codeEssaye) in
			if rep = (snd codeEssaye) then
				(List.hd codePossible) :: filtre2 algo codeEssaye (List.tl codePossible) 
			else
				filtre2 algo codeEssaye (List.tl codePossible)
		else 
			[];;

	(*Renvoie la taille de s pour un element/score donné*)
	let calculPoid element s =
		let liste = filtre2 1 element s in List.length liste;;
	
	(*incremete le score pour tester tt les possibilités*)
	let ajoutScore score =
		if (snd score) = 4 then
			((fst score) + 1, 0 )
		else
			((fst score),(snd score) + 1);;

	(*calcul le max pour un code donne*)
	let rec testToutScore element s = 
		if (fst(snd element)) < 4 then
			let score = ajoutScore (snd element) in 
			let element = ((fst element), score) in
			let poids = calculPoid element s in 
			max poids (testToutScore element s)
		else
				0;;
	(*test tout les code en fonction de tous les score possibles et ressort le minimu pour minMax*)
	let rec testToutCode score temp min s = 
		if temp < (List.length s) then
			let nouveaux = testToutScore ((List.nth s temp),score) s in
			if nouveaux < (snd min) then
				testToutCode score (temp + 1) (List.nth s temp,nouveaux) s
			else
				testToutCode score (temp + 1) min s
		else
			(fst min);;

	(* algo minMax pour optimisation choix du code a tester pour knuth*)
	let rec minMax score s =
		testToutCode score 0 ([], (List.length s)) s;;
	

	(* choisie le prochain code a tester en fonction de l'algorithme choisie (random ou minMax)*)
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

	(*filtre les codes possibles en fonction du code d'essaie et du score obtenue*)
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
	(*lance le test de l'algo de knuth en fonction d'un code rentré*)
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

	(*lance iteratio nombre de test et ressort la moyenne de coup pour que l'algo trouve la bonne reponse*)
	let rec test essaie iterations =
		if iterations >= 0 then
			let ensemble = tous in let essaie = (((fst essaie) + (testAlgo 0 ensemble ["Noir";"Noir";"Noir";"Noir"] 0)),((snd essaie) + 1)) in test essaie (iterations -1)
		else
			(float_of_int (fst essaie)) /. (float_of_int (snd essaie)) +. 1.;;
end ;;
