open Code;;
open Sys;;
open ALG_Naif;;
open ALG_Knuth;;
open IA;;

let afficherCodes codes =
  if ((List.length codes) <> 0) then
  let rec aff codes =
    match codes with
    | a :: b -> (List.iter (fun x -> print_string (x ^ " ")) a; print_string "\n";aff b;)
    | _ -> "" in
  aff codes;
  else
          "Codes vide\n";;

let () =
  Sys.command "clear";
  print_string "Bienvenue dans le MasterMind !\n\n";
  print_string "Joueur 1, veuillez rentrer le COOODEEE secret c:\n";
  Sys.command "clear";
  let secret = (Code.code_of_string (read_line()))
  and last = [] in
  let rec mainLoop i last lastTous lastReponse =
    if (i > 0) then (
            let choixFiltre = IA.filtre 0 lastReponse lastTous in (* on trie les reponses possibles *)
            let codeChoisis = IA.choix 0 last lastReponse in      (* on fait choisir l'ia *)
            let reponse = Code.reponses secret codeChoisis in
            if (reponse = (4,0)) then (
                    print_string "gagne!";
                    exit 0;
            ) else (
                    mainLoop (i - 1) (last @ [codeChoisis]) choixFiltre reponse;
            )
    ) else (
            print_string "\n\nfin\n";
            exit 0;
    )
  in mainLoop 10 last Code.tous (0,0);;
