open Draw;;
open Sdlevent;;
open Code;;
open Alg_knuth;;

let nombre_de_pion = int_of_string (Sys.argv.(1));;

let couleurs = Array.of_list ["Rouge"; "Vert"; "Bleu"; "Orange"; "Noir"; "Blanc"];;
let scores_ = Array.of_list ["Blanc"; "Noir"; "Null"];;

let find_couleur_index xs x =
  let i = ref (-1) in
  let () = Array.iteri (fun n elt -> if x = elt then i := n else ()) xs in
  !i
  
let convert_liste_to_score liste =
  (List.fold_left (fun x y -> if y = "Noir" then (x + 1) else x) 0 liste, List.fold_left (fun x y -> if y = "Blanc" then (x + 1) else x) 0 liste);;

let rec find_mouse () =
  let event = wait_event() in
  match event with
  | KEYDOWN {keysym=KEY_ESCAPE} ->
     print_endline "Merci d'avoir joué <3";
     exit 0;
  | QUIT ->
     print_endline "Merci d'avoir joué <3";
     exit 0;
  | MOUSEBUTTONDOWN e when (Draw.mouse_event e) >= 0 ->
     Draw.mouse_event e;
  | _ -> find_mouse ();;

let rec find liste liste_ref = 
  let mouse = find_mouse () in
  match mouse with
  | 0 -> liste;
  | a ->
     let pos = (find_couleur_index liste_ref (Array.get liste (a - 1))) in
     Array.set liste (a - 1) (Array.get liste_ref ((pos + 1) mod Array.length liste_ref));
     Draw.draw_interactive_pion (Array.to_list liste);
     find liste liste_ref;;

let rec wait_quit_event () =
  let event = wait_event() in
  match event with
  | KEYDOWN {keysym=KEY_ESCAPE} ->
     print_endline "Merci d'avoir joué <3";
  | QUIT ->
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
  begin
          Draw.render_text_center "pseudo du joueur 1";
          let pseudo1 = Draw.get_text (Draw.screenWidth / 2 - 10) (Draw.screenHeight / 2 + 12) in
          (pseudo1, "ORDI_1");
  end;;

let list_of_tuple t =
  [fst t; snd t];;



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

let generate_dummy_score () =
  let rec foo tmp add =
    if add > 0 then
            foo (tmp @ ["Null"]) (add - 1)
    else
            tmp;
  in foo [] nombre_de_pion;; 

let machine_vs_joueur pseudos () =
  let premier = 1 in
  let pseudos = [List.nth pseudos premier; List.nth pseudos ((premier + 1) mod 2)] in
  Draw.render_text_center_y ((List.nth pseudos 0) ^ " choisis le code") 5;
  Draw.draw_interactive_pion (generate_dummy ());
  let tous_code = Code.tous in let secret = List.nth tous_code (Random.int (List.length tous_code)) in
  Draw.draw_board_background ();
  Draw.draw_interactive_pion (generate_dummy ());
  Draw.render_text_center_y ((List.nth pseudos 1) ^ " a toi de jouer") 5;
  
  
  let essais_premier = (Array.to_list (find (Array.of_list (generate_dummy ())) couleurs)) in

  mettre_a_jour_ecran [essais_premier] [(0,0)];
  Draw.render_text_center_y ((List.nth pseudos 0) ^ " mets le score") 5;
  
  let code = Code.reponse essais_premier secret in 
  
  if (snd code) = nombre_de_pion then (*sort si limite de pion atteints mais je sais pas trop...*)
    begin
      Draw.clearScreen Draw.screen;
      Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
      Draw.render_text_center_y "escape pour sortir" 600;
      wait_quit_event ();
    end
  else
    begin
      let rec jouer vie pions scores secret =
        if vie != 0 then (*si le joueur a encore des essaies*)
          begin
            mettre_a_jour_ecran pions scores;
            Draw.draw_interactive_pion (List.hd (List.rev pions));
            Draw.render_text_center_y ((List.nth pseudos 1) ^ " a toi de jouer") 5;

						let essais = (Array.to_list (find (Array.of_list (List.hd (List.rev pions))) couleurs)) in
            let tmpPions = pions @ [essais] and tmpCode = scores @ [(0,0)] in

            mettre_a_jour_ecran tmpPions tmpCode;
            Draw.draw_interactive_pion (generate_dummy_score ());                    
            Draw.render_text_center_y ((List.nth pseudos 0) ^ " mets le score") 5;
            
            let score = Code.reponse essais secret in
            
            if (snd score) = nombre_de_pion then (*si le joueur a gagner*)
              begin
                Draw.clearScreen Draw.screen;
                Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
                Draw.render_text_center_y "escape pour sortir" 600;
                wait_quit_event ();
              end
            else (*sinon relance un tour*)
              jouer (vie - 1) tmpPions ((List.rev (List.tl (List.rev tmpCode))) @ [score]) secret
          end
        else (*si le jouer a utiliser tt ces essaies*)
          begin
            Draw.clearScreen Draw.screen;
            Draw.render_text_center ((List.nth pseudos 0) ^ " tu as gagne!");
            Draw.render_text_center_y "escape pour sortir" 600;
            wait_quit_event ();
          end
        in jouer 9 [essais_premier] [code] secret ;
    end;;


let joueur_vs_machine pseudos () =
	let premier = 0 in
	let pseudos = [List.nth pseudos premier; List.nth pseudos ((premier + 1) mod 2)] in
	Draw.render_text_center_y ((List.nth pseudos 0) ^ " choisis le code") 5;
	Draw.draw_interactive_pion (generate_dummy ());
	let secret = (Array.to_list (find (Array.of_list (generate_dummy ())) couleurs)) in

	Draw.draw_board_background ();
	Draw.draw_interactive_pion (generate_dummy ());
	Draw.render_text_center_y ((List.nth pseudos 1) ^ " a toi de jouer") 5;
  
  let essais_premier = Alg_knuth.choix 0 [] [] in	
  mettre_a_jour_ecran [essais_premier] [(0,0)];
    
	Draw.render_text_center_y ((List.nth pseudos 0) ^ " mets le score") 5;
	
	let code = (convert_liste_to_score (Array.to_list (find (Array.of_list (generate_dummy_score())) scores_))) in 
  (*c'est quoi cette fonction de code ????*)
  let vrai_code = Code.reponse essais_premier secret in
  
	if (fst vrai_code) = nombre_de_pion then 
		begin
			Draw.clearScreen Draw.screen;
			Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
			Draw.render_text_center_y "escape pour sortir" 600;
			wait_quit_event ();
		end
	else if snd code != fst vrai_code || fst code != snd vrai_code then (*si le joueur a gagner*)
    begin
      Draw.clearScreen Draw.screen;
      Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
      Draw.render_text_center_y ((List.nth pseudos 0) ^ " rentre le bon score...") 600;
      wait_quit_event ();
    end
  else
		begin
			let rec jouer vie pions scores codePossible secret=
				if vie != 0 then (*si le joueur a encore des essaies*)
					begin
						mettre_a_jour_ecran pions scores;
						Draw.draw_interactive_pion (List.hd (List.rev pions));
						Draw.render_text_center_y ((List.nth pseudos 1) ^ " a toi de jouer") 5;

						let essais = Alg_knuth.choix 1 [] codePossible in
						let tmpPions = pions @ [essais] and tmpCode = scores @ [(0,0)] in

						mettre_a_jour_ecran tmpPions tmpCode;
						Draw.draw_interactive_pion (generate_dummy_score ());                    
						Draw.render_text_center_y ((List.nth pseudos 0) ^ " mets le score") 5;
						
						let score = (convert_liste_to_score (Array.to_list (find (Array.of_list (generate_dummy_score ())) scores_))) in
            let vrai_code = Code.reponse essais secret in

            
						if (snd score) = nombre_de_pion then (*si le joueur a gagner*)
							begin
								Draw.clearScreen Draw.screen;
								Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
								Draw.render_text_center_y "escape pour sortir" 600;
								wait_quit_event ();
              end
            else if snd score != fst vrai_code || fst score != snd vrai_code then (*si le joueur a gagner*)
              begin
                Draw.clearScreen Draw.screen;
                Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
                Draw.render_text_center_y ((List.nth pseudos 0) ^ " rentre le bon score...") 600;
                wait_quit_event ();
              end
						else (*sinon relance un tour*)
							let codePossible = Alg_knuth.filtre 1 (essais,(snd score,fst score)) codePossible in
							jouer (vie - 1) tmpPions ((List.rev (List.tl (List.rev tmpCode))) @ [score]) codePossible secret
					end
				else (*si le jouer a utiliser tt ces essaies*)
					begin
						Draw.clearScreen Draw.screen;
						Draw.render_text_center ((List.nth pseudos 0) ^ " tu as gagne!");
						Draw.render_text_center_y "escape pour sortir" 600;
						wait_quit_event ();
					end
				in jouer 9 [essais_premier] [code] (Alg_knuth.filtre 1 (essais_premier,(snd code,fst code)) Code.tous) secret;
		end;;

let joueur_vs_joueur pseudos () =
  let premier = (Random.int 2) in
  let pseudos = [List.nth pseudos premier; List.nth pseudos ((premier + 1) mod 2)] in
  Draw.render_text_center_y ((List.nth pseudos 0) ^ " choisis le code") 5;
  Draw.draw_interactive_pion (generate_dummy ());
  let secret = (Array.to_list (find (Array.of_list (generate_dummy ())) couleurs)) in

  Draw.draw_board_background ();
  Draw.draw_interactive_pion (generate_dummy ());
  Draw.render_text_center_y ((List.nth pseudos 1) ^ " a toi de jouer") 5;
  
  let essais_premier = (Array.to_list (find (Array.of_list (generate_dummy ())) couleurs)) in

  mettre_a_jour_ecran [essais_premier] [(0,0)];
  Draw.render_text_center_y ((List.nth pseudos 0) ^ " mets le score") 5;
  
	let code = (convert_liste_to_score (Array.to_list (find (Array.of_list (generate_dummy_score())) scores_))) in 
	(*c'est quoi cette fonction de code ????*)
  
  if (snd code) = nombre_de_pion then (*sort si limite de pion atteints mais je sais pas trop...*)
    begin
			Draw.clearScreen Draw.screen;
			Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
			Draw.render_text_center_y "escape pour sortir" 600;
			wait_quit_event ();
    end
  else
    begin
      let rec jouer vie pions scores =
        if vie != 0 then (*si le joueur a encore des essaies*)
          begin
						mettre_a_jour_ecran pions scores;
						Draw.draw_interactive_pion (List.hd (List.rev pions));
						Draw.render_text_center_y ((List.nth pseudos 1) ^ " a toi de jouer") 5;
						
						let essais = (Array.to_list (find (Array.of_list (List.hd (List.rev pions))) couleurs)) in
						let tmpPions = pions @ [essais] and tmpCode = scores @ [(0,0)] in

						mettre_a_jour_ecran tmpPions tmpCode;
						Draw.draw_interactive_pion (generate_dummy_score ());                    
						Draw.render_text_center_y ((List.nth pseudos 0) ^ " mets le score") 5;
						
						let score = (convert_liste_to_score (Array.to_list (find (Array.of_list (generate_dummy_score ())) scores_))) in
						
						if (snd score) = nombre_de_pion then (*si le joueur a gagner*)
							begin
								Draw.clearScreen Draw.screen;
								Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
								Draw.render_text_center_y "escape pour sortir" 600;
								wait_quit_event ();
							end
						else (*sinon relance un tour*)
							jouer (vie - 1) tmpPions ((List.rev (List.tl (List.rev tmpCode))) @ [score])
					end
				else (*si le jouer a utiliser tt ces essaies*)
					begin
						Draw.clearScreen Draw.screen;
						Draw.render_text_center ((List.nth pseudos 0) ^ " tu as gagne!");
						Draw.render_text_center_y "escape pour sortir" 600;
						wait_quit_event ();
					end
					in jouer 9 [essais_premier] [code]; (*JSP a quoi ca sert...*)
		end;;

let () =
  Random.self_init ();
  Draw.init_draw_module "Mastermind - Paul & Thomas";

  let type_de_partie = Draw.menu_type_de_partie "Comment voulez vous jouer?" ["JvJ"; "OvJ"; "JvO"] in
  let pseudos = trouver_pseudo type_de_partie in
  
  Draw.draw_board_background ();

  match type_de_partie with
  | 0 -> joueur_vs_joueur (list_of_tuple pseudos) ();
  | 1 -> joueur_vs_machine (list_of_tuple pseudos) ();
  | 2 -> machine_vs_joueur (list_of_tuple pseudos) ();
  
  Sdltimer.delay 10;;
                       
