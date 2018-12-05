open Code;;
open Sys;;

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
  let secret = (Code.code_of_string (read_line())) and last = [] in
  let rec mainLoop i last =
    if (i > 0) then (
            print_string "Les anciens codes sont :\n";
            afficherCodes last;
            print_string "Votre idee ?\n";
            let code = Code.code_of_string (read_line()) in
            if (Code.compare code secret) = 0 then (
                    print_string "gagne!\n";
                    mainLoop 0 last;
            ) else (
                    print_string "nop.\n";
                    let reponse = (Code.reponse code secret) in
                    print_string ("Les resultats sont : bien place :" ^ string_of_int(fst reponse) ^ ", mal place : " ^ string_of_int (snd reponse) ^ "\n"));
            mainLoop (i - 1) (last @ [code]);
    ) else (
            print_string "\n\nfin\n";
            exit 0;
    )
  in mainLoop 10 last;;
          
