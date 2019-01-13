open Sdlevent;;
open Draw;;

let couleurs = Array.of_list ["Rouge"; "Bleu"; "Cyan"; "Fonce"; "Blanc"; "Noir"; "Jaune"; "Orange"; "Rose"; "Vert"; "Violet"];;

let rec wait_for_event () = 
  let event = wait_event () in
  match event with
  | KEYDOWN {keysym=KEY_ESCAPE} ->
     print_endline "Merci d'avoir joué <3"
  | MOUSEBUTTONDOWN e ->
     print_int (Draw.mouse_event e);
     print_endline "";
     wait_for_event ()
  | _ ->
     wait_for_event ()

let find_couleur_index xs x =
  let i = ref (-1) in
  let () = Array.iteri (fun n elt -> if x = elt then i := n else ()) xs in
  !i
  
let rec find_couleur_to_play liste = 
  let event = wait_event () in
  match event with
  | KEYDOWN {keysym=KEY_ESCAPE} ->
     print_endline "Merci d'avoir joué <3"
  | MOUSEBUTTONDOWN e ->
     let pos = find_couleur_index liste (Draw.mouse_event e) in
     if (Draw.mouse_event e) = 0 then
             begin
                     liste;
             end
     else
             begin
                     Array.set liste ((Draw.mouse_event e) - 1) (Array.get couleurs (pos + 1));
                     find_couleur_to_play liste;
             end
  | _ ->
     find_couleur_to_play liste;;

  
let () =
  Draw.init_draw_module "Mastermind - Paul & Thomas";
  Draw.draw_board_background;
  Draw.draw_interactive_pion ["Vert"; "Bleu"; "Rose"; "Blanc"; "Rouge"];
  Draw.draw_interactive_pion (find_couleur_to_play (Array.make Draw.nombre_de_pion "Rouge"));
  wait_for_event ();;
  
  
