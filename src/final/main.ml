open Draw;;
open Sdlevent;;
open Code;;
open Alg_knuth;;
open Alg_genetic;;

let nombre_de_pion = int_of_string (Sys.argv.(1));;
let nombre_de_couleurs = int_of_string (Sys.argv.(2));;


(* let couleurs = Array.of_list ["Rouge"; "Vert"; "Bleu"; "Orange"; "Noir"; "Blanc"];;*)

let couleurs = Array.of_list ["Vert"; "Rouge"; "Bleu"; "Orange"; "Noir"; "Blanc"; "Cyan"; "Fonce"; "Rose";"Jaune" ;"Violet" ];;
let scores_ = Array.of_list ["Blanc"; "Noir"; "Null"];;


(** ajustement_couleurs
* @param nombreDePions varaibleTemporaire
* @return la liste de pion a la taille du parametre 
*)
let rec ajustement_couleurs param temp =
  if param > 0 then
    (Array.get couleurs temp) :: ajustement_couleurs (param - 1) (temp + 1)
  else 
    [];;

let couleurs = Array.of_list (ajustement_couleurs nombre_de_couleurs 0);;


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

(** code_random
* @param longueur du code voulu
* @return un code rendome a la taille voulu
*)
let rec code_random temp =
  if temp > 0 then
    (Array.get couleurs (Random.int (Array.length couleurs))) :: code_random (temp - 1)   
  else
    [];;

(** joue partie ou la machine choisie le code et le joueur la devine 
* @param pseudos des joeurs
* @return unit 
*)

let machine_vs_joueur pseudos () =
	Draw.draw_board_background ();
  let premier = 1 in
  let pseudos = [List.nth pseudos premier; List.nth pseudos ((premier + 1) mod 2)] in
  Draw.render_text_center_y ((List.nth pseudos 0) ^ " choisis le code") 5;
  Draw.draw_interactive_pion (generate_dummy ());
  let secret = code_random nombre_de_pion in
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

    let rec affiche_code codes =
      if codes != [] then
        (print_string (Code.string_of_code (List.hd codes));
        print_string "  \n";  
        affiche_code (List.tl codes);)
      else
        print_string "fini";;



(** joue partie ou le joueur choisie le code et la machine le devine 
* @param pseudos des joeurs
* @param algo choisie par le joueur
* @return unit 
*)

let rec test_dedans secret listeCode =
  if listeCode != [] then
    if (List.hd listeCode) = secret then
      print_string "ok"
    else
      test_dedans secret (List.tl listeCode)
  else
    print_string "nope..";;

let _joueur_vs_machine pseudos algo () =
  Draw.draw_board_background ();
  let premier = 0 in
  let pseudos = [List.nth pseudos premier; List.nth pseudos ((premier + 1) mod 2)] in
  Draw.render_text_center_y ((List.nth pseudos 0) ^ " choisis le code") 5;
  Draw.draw_interactive_pion (generate_dummy ());
  let secret = (Array.to_list (find (Array.of_list (generate_dummy ())) couleurs)) in

  Draw.draw_board_background ();
  Draw.draw_interactive_pion (generate_dummy ());
  Draw.render_text_center_y ((List.nth pseudos 1) ^ " a toi de jouer") 5;
  

  let essais_premier = Alg_knuth.choix 1 [] (Code.tous) in
  mettre_a_jour_ecran [essais_premier] [(0,0)];

  
  Draw.render_text_center_y ((List.nth pseudos 0) ^ " mets le score") 5;
  
  let code = (convert_liste_to_score (Array.to_list (find (Array.of_list (generate_dummy_score())) scores_))) in 
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
		  let rec jouer vie pions scores codePossible secret algo =
		    if vie != 0 then (*si le joueur a encore des essaies*)
          begin
				    mettre_a_jour_ecran pions scores;
				    Draw.draw_interactive_pion (List.hd (List.rev pions));
				    Draw.render_text_center_y ((List.nth pseudos 1) ^ " a toi de jouer") 5;
            

				    let essais = Alg_knuth.choix algo [] codePossible in
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
					    let codePossible = Alg_knuth.filtre algo (essais,(vrai_code)) codePossible in
					    jouer (vie - 1) tmpPions ((List.rev (List.tl (List.rev tmpCode))) @ [score]) codePossible secret algo
			    end
		    else (*si le jouer a utiliser tt ces essaies*)
			    begin
				    Draw.clearScreen Draw.screen;
				    Draw.render_text_center ((List.nth pseudos 0) ^ " tu as gagne!");
				    Draw.render_text_center_y "escape pour sortir" 600;
				    wait_quit_event ();
			    end
		  in jouer 9 [essais_premier] [code] (Alg_knuth.filtre algo (essais_premier,vrai_code) Code.tous) secret algo;
	  end;;

let fonction_score pions score pseudo =
  let _score = List.split score in
  let score = List.combine (snd _score) (fst _score) in
  mettre_a_jour_ecran pions score;
  Draw.draw_interactive_pion (generate_dummy_score ());                    
  Draw.render_text_center_y (pseudo ^ " mets le score") 5;

  let tmp = (convert_liste_to_score (Array.to_list (find (Array.of_list (generate_dummy_score ())) scores_))) in
  (snd tmp, fst tmp);;
  
(** joue partie ou le joueur choisie le code et la machine le devine 
* @param pseudos des joeurs
* @param algo choisie par le joueur
* @return unit 
*)

let joueur_vs_machine pseudos algo () =
  if ((algo = 0) || (algo = 2)) then
          _joueur_vs_machine pseudos algo ()
  else
          begin
                  Random.self_init ();
                  Draw.draw_board_background ();
                  let premier = 0 in
                  let pseudos = [List.nth pseudos premier; List.nth pseudos ((premier + 1) mod 2)] in
                  Draw.render_text_center_y ((List.nth pseudos 0) ^ " choisis le code") 5;
                  Draw.draw_interactive_pion (generate_dummy ());
                  let secret = (Array.to_list (find (Array.of_list (generate_dummy ())) couleurs)) in

                  let code_depart = ["Rouge"; "Rouge"; "Vert"; "Vert"] in

                  let result = fonction_score [code_depart] [(0,0)] (List.nth pseudos 0) in

                  if (fst result) != nombre_de_pion then
                          begin
                                  (* pas encore fait le truc du res faux *)
                                  let rec jouer vie pions score last_code =
                                    if vie != 0 then
                                            begin
                                                    mettre_a_jour_ecran pions score;
                                                    let move_ia = Alg_genetic.make_gen_move (List.combine pions score) last_code in
                                                    let res = fonction_score (pions @ [move_ia]) (score @ [(0,0)]) (List.nth pseudos 0) in

                                                    if (fst res) = nombre_de_pion then
                                                            begin
								    Draw.clearScreen Draw.screen;
								    Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
								    Draw.render_text_center_y "escape pour sortir" 600;
								    wait_quit_event ();                                                                    
                                                            end
                                                    else
                                                            begin
                                                                    jouer (vie - 1) (pions @ [move_ia]) (score @ [res]) move_ia
                                                            end
                                            end
                                    else
                                            begin
                                                    Draw.clearScreen Draw.screen;
						    Draw.render_text_center ((List.nth pseudos 0) ^ " tu as gagne!");
						    Draw.render_text_center_y "escape pour sortir" 600;
						    wait_quit_event ();                                                                    
                                            end
                                  in jouer 9 [code_depart] [result] code_depart;
                          end
                  else
                          begin
                                  Draw.clearScreen Draw.screen;
				  Draw.render_text_center ((List.nth pseudos 1) ^ " tu as gagne!");
				  Draw.render_text_center_y "escape pour sortir" 600;
				  wait_quit_event ();
                          end
          end;;
  
(** joue partie ou un joueur choisie le code et l'autre le devine 
* @param pseudos des joeurs
* @return unit 
*)

let joueur_vs_joueur pseudos () =
  Draw.draw_board_background ();
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
					in jouer 9 [essais_premier] [code]; 
    end;;
    
let rec ajustement_couleurs param temp =
  if param > 0 then
    (Array.get couleurs temp) :: ajustement_couleurs (param - 1) (temp + 1)
  else 
    [];;

(** fonction main pour joueur une partie 
* @return unit 
*)

let () =
  Random.self_init ();

  let couleurs = Array.of_list (ajustement_couleurs nombre_de_couleurs 0) in
  let couleur_possibles = Array.to_list couleurs in 
  Draw.init_draw_module "Mastermind - Paul & Thomas";

  let type_de_partie = Draw.menu_type_de_partie "Comment voulez vous jouer?" ["JvJ"; "OvJ"; "JvO"] in
  let pseudos = trouver_pseudo type_de_partie in

  Draw.clearScreen Draw.screen;

  match type_de_partie with
  | 0 -> joueur_vs_joueur (list_of_tuple pseudos) ();
  | 1 -> joueur_vs_machine (list_of_tuple pseudos) (Draw.menu_type_de_partie "Quel algo veux tu ?" ["Knuth"; "Genetic"; "megaFiltre"]) ();
  | 2 -> machine_vs_joueur (list_of_tuple pseudos) ();
  
  Sdltimer.delay 10;;
                       
