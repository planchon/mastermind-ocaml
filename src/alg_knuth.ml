(** Algorithmes de recherche de code *)
module IA :
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
		
		val test : unit -> int

end = struct

	let nombre_methodes = 2;;

	open Code;;

	let rec filtre2 algo codeEssaye codePossible  = 
		if codePossible != [] && algo = 1 then
			let reponse = reponse (List.hd codePossible) (fst codeEssaye) in
			if reponse = (snd codeEssaye) then
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

	let rec testToutCode score temp min s = 
		if temp < (List.length s) then
			let nouveaux = testToutScore ((List.nth s temp),score) s in
			if nouveaux < (snd min) then
				testToutCode score (temp + 1) (List.nth s temp,nouveaux) s
			else
				testToutCode score (temp + 1) min s
		else
			(fst min);;

	let rec minMax score s =
		testToutCode score 0 ([], (List.length s)) s;;
	
	let choix algo codeDeja codePossible =
		if algo = 0 then
			List.nth codePossible (Random.int (List.length codePossible))
		else	
			minMax (0,0) codePossible;;
 
	let rec filtre algo codeEssaye codePossible  = 
		if codePossible != [] && algo = 1 then
			let reponse = reponse (List.hd codePossible) (fst codeEssaye) in
			if reponse = (snd codeEssaye) then
				(List.hd codePossible) :: filtre algo codeEssaye (List.tl codePossible) 
			else
				filtre algo codeEssaye (List.tl codePossible) 
		else 
			[];;

	let rec testAlgo numero listeCode codeSecret temp =
		let codeTest = choix numero [] listeCode in
		let listeCode = filtre 1 (codeTest,reponse codeTest codeSecret) listeCode in
		if (List.length listeCode) <= 1 then
			temp
		else
			testAlgo 1 listeCode codeSecret (temp + 1);;

	let test () = 
		let ensemble = tous in testAlgo 0 ensemble ["Vert";"Blanc";"Noir";"Bleu"] 0;;

end ;;