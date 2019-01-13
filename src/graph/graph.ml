open Sdlevent;;
open Draw;;

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

let () =
  Draw.init_draw_module "Mastermind - Paul & Thomas";
  Draw.draw_board_background;
  Draw.draw_pions [["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]; ["Vert"; "Vert"; "Vert"; "Vert"]];
  Draw.draw_score [(0, 1); (0, 2); (0, 4); (1, 3); (2, 2); (0,0)];
  Sdltimer.delay 1000;;
  (* print_list (Array.to_list (find_couleur (Array.of_list ["Vert"; "Bleu"; "Rose"; "Blanc"])));
   * print_score (convert_liste_to_score (Array.to_list (find_score (Array.of_list ["Noir"; "Null"; "Null"; "Null"]))));; *)

  
