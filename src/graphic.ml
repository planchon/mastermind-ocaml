module Graphic :
sig 
  val ecranDepart : unit -> Code.t
end = struct
  (*
  #load "graphics.cma" ;;
  *)
  open Graphics;;
  open IA;;
  open Code;;
  
  let orange 	= rgb 198 141 62 ;;
  let rouge 	= rgb 255 0 0;;
  let vert	  = rgb 0 255 0;;
  let bleu	  = rgb 0 0 255;; 
  let noir 	  = rgb 0 0 0;;
  let blanc 	= rgb 255 255 255;;
  
  let drawPion coorx coory rayon couleur =
    set_color couleur;
    fill_circle coorx coory rayon;
    set_color noir;;
  
  
  let string_to_color pion =
    match pion with
    | "Rouge" 	-> rouge
    | "Vert"	  -> vert
    | "Bleu"	  -> bleu
    | "Orange"	-> orange
    | "Noir"	  -> noir
    | "Blanc"	  -> blanc
    | _         -> blanc;;
  
  let rec drawCode coorx coory code delta rayon=
    let couleur = string_to_color (List.hd code) in drawPion coorx coory rayon couleur;
    if List.tl code != [] then drawCode (coorx + delta) coory (List.tl code) delta rayon;;
  
  let map_selection coorx col =
    let tailleCarre = 500/col in (coorx - 250) / tailleCarre;;
  
  let changement_couleur coul =
    match coul with
    | "Rouge" 	-> "Vert"
    | "Vert"	  -> "Bleu"
    | "Bleu"	  -> "Orange"
    | "Orange"	-> "Noir"
    | "Noir"	  -> "Blanc"
    | "Blanc"	  -> "Rouge"
    | _         -> "Blanc";;
  
  let rec changement_code position codeSelection =
    if position = 0 then
      (changement_couleur (List.hd codeSelection)) :: (List.tl codeSelection)
    else
      (List.hd codeSelection) :: (changement_code (position-1) (List.tl codeSelection));;
  
  
  
  let drawGrille col row ecart =
    set_color black;
    moveto 250 200;
    (*grille*)
    for i = 0 to col do
      lineto (current_x ()) (current_y () + 800);
      moveto (current_x () + (fst ecart)) (current_y () - 800);
    done;
    moveto 250 200;
    for i = 0 to row do
      lineto (current_x () + 500) (current_y ()); 
      moveto (current_x () - 500) (current_y () + (snd ecart));
    done;
    (*selection*)
    moveto 250 50;
    for i = 0 to 1 do
      lineto (current_x () + 500) (current_y ()); 
      moveto (current_x () - 500) (current_y () + 100);
    done;
    moveto 250 50;
    for i = 0 to col do
      lineto (current_x () ) (current_y () + 100); 
      moveto (current_x () + (fst ecart) ) (current_y () - 100);
    done;
    (*Bonnes reponses*)
    moveto 50 200;
    for i = 0 to row do
      lineto (current_x () + 100) (current_y ()); 
      moveto (current_x () - 100) (current_y () + (snd ecart));
    done;
    moveto 50 200;
    for i = 0 to 1 do
      lineto (current_x () ) (current_y () + 800); 
      moveto (current_x () + 100 ) (current_y() - 800);
    done;;
  
  let draw_bonne_reponse reponse1 reponse2 range row =
    moveto 100 (200 + (800 / row) * range + ((800 / row)/2));
    let reponse = string_of_int reponse1 ^ " " ^ string_of_int reponse2 in draw_string (reponse);;
  
  
  let rec draw_all_code liste_code numero col row =
    drawCode (250 + ((500/col)/2)) (200 + ((800/row)/2) + ((800/row)*numero)) (snd (List.hd liste_code)) (500/col) ((min (500/col) (800/row))/2 - 2);
    draw_bonne_reponse (fst (fst (List.hd liste_code))) (snd (fst (List.hd liste_code))) numero row;
    if (List.tl liste_code) != [] then
      draw_all_code (List.tl liste_code) (numero + 1) col row;;
  
  let rec ecranJeu col row listeCode codeSelection codeSecret =
    clear_graph ();
    set_color red;
    fill_rect 50 50 100 75;
    drawGrille col row (500 / col , 800 / row);
    if listeCode != [] then
      draw_all_code listeCode 0 col row;
    drawCode (250 + ((500/col)/2)) (50 + 50) codeSelection (500/col) ((min (500/col) (100))/2 - 2);
    let codeSelection = selectionCouleur codeSecret listeCode codeSelection col row in ecranJeu col row listeCode codeSelection codeSecret;

  and selectionCouleur codeSecret listeCode codeSelection col row =
    let e = Graphics.wait_next_event [Graphics.Button_down] in
    if e.Graphics.mouse_x > 250 && e.Graphics.mouse_x < 750 && e.Graphics.mouse_y > 50 && e.Graphics.mouse_y < (50 + (800/row)) then
      let position = map_selection e.Graphics.mouse_x col in changement_code position codeSelection
    else if e.Graphics.mouse_x > 50 && e.Graphics.mouse_x < 150 && e.Graphics.mouse_y > 50 && e.Graphics.mouse_y < 125 then 
      let rep = Code.reponse codeSelection codeSecret in ecranJeu col row ((rep,codeSelection) :: listeCode) codeSelection codeSecret
    else
      codeSelection;;

  (*ecran de Jeu pour IA *)
  let rec ecranJeuIA col row listeCode codeSelection codeSecret s =
    clear_graph ();
    set_color red;
    fill_rect 50 50 100 75;
    drawGrille col row (500 / col , 800 / row);
    if listeCode != [] then
      draw_all_code listeCode 0 col row;
    drawCode (250 + ((500/col)/2)) (50 + 50) codeSelection (500/col) ((min (500/col) (100))/2 - 2);
    let codeSelection = selectionCouleurIA codeSecret listeCode codeSelection col row s in ecranJeuIA col row listeCode codeSelection codeSecret s;

  and selectionCouleurIA codeSecret listeCode codeSelection col row s=
    let e = Graphics.wait_next_event [Graphics.Button_down] in
    if e.Graphics.mouse_x > 50 && e.Graphics.mouse_x < 150 && e.Graphics.mouse_y > 50 && e.Graphics.mouse_y < 125 then 
      let code = IA.choix 0 [] s in let rep = Code.reponse code codeSecret in let s = IA.filtre 1 (code,rep) s in ecranJeuIA col row ((rep,code) :: listeCode) code codeSecret s
    else
      codeSelection;;

  let rec codeRandome acc =
      if acc < 3 then
        let possible = ["Vert";"Bleu";"Orange";"Noir";"Blanc";"Rouge";"Blanc"] in
        let nombre = Random.int (List.length possible) in (List.nth possible nombre) :: codeRandome (acc + 1)
      else
        [];;


  let attentInput () =
    let e = Graphics.wait_next_event [Graphics.Key_pressed] in
    if e.Graphics.keypressed then 
      let key = e.Graphics.key in
      if key = 'o' then let test = ecranJeu 4 8 [] ["Noir";"Noir";"Noir";"Noir"] ["Noir";"Noir";"Noir";"Noir"] in () 
      else if key = 'n' then let code = codeRandome 0 in let test = ecranJeu 10 10 [] ["Noir";"Noir";"Noir"] code in () 
      else Graphics.close_graph ();;
  
  let rec codeNoir tmp =
    if tmp != 0 then 
      "Noir" :: codeNoir (tmp - 1)
    else 
      [];;

  let rec choisieCode code col row =
    clear_graph ();
    set_color red;
    fill_rect 50 50 100 75;
    set_color black;
    set_color black;
    moveto 250 400;
    (*grille*)
    for i = 0 to col do
      lineto (current_x ()) (current_y () + 75);
      moveto (current_x () + (500 / col)) (current_y () - 75);
    done;
    moveto 250 400;
    for i = 0 to 1 do
      lineto (current_x () + 500) (current_y ()); 
      moveto (current_x () - 500) (current_y () + 75);
    done;
    let delta = (500 / col) in drawCode (250 + delta/2) (438) code delta 30;
    let code = selectionCouleurChoisie code col row in choisieCode code col row;
    
  and selectionCouleurChoisie codeSelection col row =
    let e = Graphics.wait_next_event [Graphics.Button_down] in
    if e.Graphics.mouse_x > 250 && e.Graphics.mouse_x < 750 && e.Graphics.mouse_y > 400 && e.Graphics.mouse_y < 475 then
      let position = map_selection e.Graphics.mouse_x col in changement_code position codeSelection
    else if e.Graphics.mouse_x > 50 && e.Graphics.mouse_x < 150 && e.Graphics.mouse_y > 50 && e.Graphics.mouse_y < 125 then 
      ecranJeu col row [] (codeNoir col) codeSelection
    else
      codeSelection;;

  let rec drawNumero coorx coory col delta tmp =
    moveto (coorx) (coory);
    draw_string (string_of_int tmp);
    if col != 1 then
      drawNumero (coorx + delta) coory (col - 1) delta (tmp + 1);;

  let rec choisierow col row =
    clear_graph ();
    set_color black;
    moveto 250 750;
    draw_string "choisie un nbr de lignes stp";
    moveto 250 400;
    (*grille*)
    for i = 0 to row do
      lineto (current_x ()) (current_y () + 75);
      moveto (current_x () + (500 / row)) (current_y () - 75);
    done;
    moveto 250 400;
    for i = 0 to 1 do
      lineto (current_x () + 500) (current_y ()); 
      moveto (current_x () - 500) (current_y () + 75);
    done;
    let delta = (500 / row) in drawNumero (250 + delta/2) (438) row delta 1;
    selectionrowChoisie col row;
    
  and selectionrowChoisie col row =
    let e = Graphics.wait_next_event [Graphics.Button_down] in
    if e.Graphics.mouse_x > 250 && e.Graphics.mouse_x < 750 && e.Graphics.mouse_y > 400 && e.Graphics.mouse_y < 475 then
      let row = map_selection e.Graphics.mouse_x row in choisieCode (codeNoir col) col (row + 1) 
    else
      selectionrowChoisie col row;;

  let rec choisiecol col =
    clear_graph ();
    set_color black;
    moveto 250 750;
    draw_string "choisie un nbr de colones stp";
    moveto 250 400;
    (*grille*)
    for i = 0 to col do
      lineto (current_x ()) (current_y () + 75);
      moveto (current_x () + (500 / col)) (current_y () - 75);
    done;
    moveto 250 400;
    for i = 0 to 1 do
      lineto (current_x () + 500) (current_y ()); 
      moveto (current_x () - 500) (current_y () + 75);
    done;
    let delta = (500 / col) in drawNumero (250 + delta/2) (438) col delta 1;
    selectioncolChoisie col;
    
  and selectioncolChoisie col =
    let e = Graphics.wait_next_event [Graphics.Button_down] in
    if e.Graphics.mouse_x > 250 && e.Graphics.mouse_x < 750 && e.Graphics.mouse_y > 400 && e.Graphics.mouse_y < 475 then
      let col = map_selection e.Graphics.mouse_x col in choisierow (col + 1) 10 
    else
      selectioncolChoisie col;;

  let ecranDepart () =
    open_graph "";
    set_window_title "thomas c'est le plus fort";
    resize_window 1000 1200;
    moveto 450 500;
    draw_string "coucou t'es pret a jouer";
    moveto 450 450;
    draw_string "o pour creer un code n pour deviner un code";
    let s = tous in ecranJeuIA 4 5 [] ["Noir";"Noir";"Noir";"Noir"] ["Vert";"Blanc";"Noir";"Bleu"] s ;;
    (* choisiecol 10;; *)
    (* attentInput ();; *)

end;;