open Draw;;
open Sdlevent;;

let nombre_de_pion = int_of_string (Sys.argv.(1));;

let couleurs = Array.of_list ["Rouge"; "Bleu"; "Cyan"; "Fonce"; "Jaune"; "Orange"; "Rose"; "Vert"; "Violet"];;
let scores = Array.of_list ["Blanc"; "Noir"; "Null"];;

let find_couleur_index xs x =
  let i = ref (-1) in
  let () = Array.iteri (fun n elt -> if x = elt then i := n else ()) xs in
  !i
  
let print_list liste =
  for i=0 to ((List.length liste) - 1) do
          print_string (" " ^ (List.nth liste i));
  done;
  print_endline "";;

let convert_liste_to_score liste =
  (List.fold_left (fun x y -> if y = "Noir" then (x + 1) else x) 0 liste, List.fold_left (fun x y -> if y = "Blanc" then (x + 1) else x) 0 liste);;

let print_score score =
  print_int (fst score);
  print_int (snd score);;

let rec find_score liste = 
  let event = wait_event() in
  match event with
  | MOUSEBUTTONDOWN e ->
     if (Draw.mouse_event e) = 0 then
             begin
                     liste;
             end
     else
             begin
                     let pos = (find_couleur_index scores (Array.get liste ((Draw.mouse_event e) - 1))) in
                     Array.set liste ((Draw.mouse_event e) - 1) (Array.get scores ((pos + 1) mod Array.length scores));
                     Draw.draw_interactive_pion (Array.to_list liste);
                     find_score liste;
             end     
  | _ ->
     find_score liste;;

let rec find_couleur liste =
  let event = wait_event() in
  match event with
  | MOUSEBUTTONDOWN e ->
     if (Draw.mouse_event e) = 0 then
             begin
                     liste;
             end
     else
             begin
                     let pos = (find_couleur_index couleurs (Array.get liste ((Draw.mouse_event e) - 1))) in
                     Array.set liste ((Draw.mouse_event e) - 1) (Array.get couleurs ((pos + 1) mod Array.length couleurs));
                     Draw.draw_interactive_pion (Array.to_list liste);
                     find_couleur liste;
             end     
  | _ ->
     find_couleur liste;;

let rec wait_quit_event () =
  let event = wait_event() in
  match event with
  | KEYDOWN {keysym=KEY_ESCAPE} ->
     print_endline "Merci d'avoir joué <3";
  | _ ->
     wait_quit_event ();;

let trouver_pseudo partie=
  Draw.clearScreen Draw.screen;
  match partie with
  | 0 ->
     begin
             Draw.render_text_center "pseudo du joueur 1";
             let pseudo1 = Draw.get_text (Draw.screenWidth / 2 - 10) (Draw.screenHeight / 2 + 12) in
             Draw.clearScreen Draw.screen;
             Draw.render_text_center "pseudo du joueur 2";
             let pseudo2 = Draw.get_text (Draw.screenWidth / 2 - 10) (Draw.screenHeight / 2 + 12) in
             (pseudo1, pseudo2);
     end
  | 1 ->
     begin
             Draw.render_text_center "pseudo du joueur 1";
             let pseudo1 = Draw.get_text (Draw.screenWidth / 2 - 10) (Draw.screenHeight / 2 + 12) in
             (pseudo1, "ORDI_1");
     end
  | 2 ->
     ("ORDI_1", "ORDI_2");;

let list_of_tuple t =
  [fst t; snd t];;

let joueur_vs_machine () =
  print_int 1;;

let mettre_a_jour_ecran pion score =
    Draw.draw_board_background ();
    Draw.draw_pions pion;
    Draw.draw_score score;;

let generate_dummy () =
  let rec foo tmp add =
    if add > 0 then
            foo (tmp @ ["Vert"]) (add - 1)
    else
            tmp;
  in foo [] nombre_de_pion;;
  

let joueur_vs_joueur pseudos () =
  let premier = (Random.int 2) in
  let pseudos = [List.nth pseudos premier; List.nth pseudos ((premier + 1) mod 2)] in
  Draw.render_text_center ((List.nth pseudos 0) ^ " choisis le code");
  Sdltimer.delay 750;
  Draw.draw_board_background ();
  Draw.draw_interactive_pion (generate_dummy ());
  let secret = (Array.to_list (find_couleur (Array.of_list (generate_dummy ())))) in

  let rec jouer vie pions scores =
    mettre_a_jour_ecran pions scores;
    Draw.draw_interactive_pion (List.hd (List.rev pions));
    Draw.render_text_center ((List.nth pseudos 1) ^ " a toi de jouer");
    Sdltimer.delay 1500;
    mettre_a_jour_ecran pions scores;
    
    let essais = (Array.to_list (find_couleur (Array.of_list (List.hd (List.rev pions))))) in
    let tmpPions = pions @ [essais] and tmpCode = scores @ [(0,0)] in
    
    Draw.render_text_center ((List.nth pseudos 0) ^ " mets le score");
    mettre_a_jour_ecran tmpPions tmpCode;
    Draw.draw_interactive_pion secret;
    Sdltimer.delay 1500;
    
    let code = (convert_liste_to_score (Array.to_list (find_score (Array.of_list ["Null"; "Null"; "Null"; "Null"])))) in
    
    if (snd code) = nombre_de_pion then
            begin
                    Draw.clearScreen Draw.screen;
                    Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
                    wait_quit_event ();
            end
    else
            jouer (vie - 1) tmpPions ((List.rev (List.tl (List.rev tmpCode))) @ [code])
  in jouer 9 [] [];;
  
let () =
  Random.self_init ();
  Draw.init_draw_module "Mastermind - Paul & Thomas";

  let type_de_partie = Draw.menu_type_de_partie "Comment voulez vous jouer?" ["JvJ"; "OvJ"; " "] in
  let pseudos = trouver_pseudo type_de_partie in
  
  Draw.draw_board_background ();

  match type_de_partie with
  | 0 -> joueur_vs_joueur (list_of_tuple pseudos) ();
  | 1 -> joueur_vs_machine ();  
  
  Sdltimer.delay 10;;
                       
