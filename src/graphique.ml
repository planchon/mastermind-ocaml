#load "graphics.cma" ;;

open Graphics;;

open_graph "";;
set_window_title "thomas c'est le plus fort";;
resize_window 1000 1200;;

let orange 	= rgb 198 141 62 ;;
let rouge 	= rgb 255 0 0;;
let vert	= rgb 0 255 0;;
let bleu	= rgb 0 0 255;; 
let noir 	= rgb 0 0 0;;
let blanc 	= rgb 255 255 255;;

let drawPion coorx coory rayon couleur =
	set_color couleur;
	fill_circle coorx coory rayon;
	set_color noir;;


let string_to_color pion =
	match pion with
	| "Rouge" 	-> rouge
	| "Vert"	-> vert
	| "Bleu"	-> bleu
	| "Orange"	-> orange
	| "Noir"	-> noir
	| "Blanc"	-> blanc;;

let rec drawCode coorx coory code delta rayon=
	let couleur = string_to_color (List.hd code) in drawPion coorx coory rayon couleur;
	if List.tl code != [] then drawCode (coorx + delta) coory (List.tl code) delta rayon;;

let map_selection coorx col =
	let tailleCarre = 500/col in (coorx - 250) / tailleCarre;;

let changement_couleur coul =
	match coul with
	| "Rouge" 	-> "Vert"
	| "Vert"	-> "Bleu"
	| "Bleu"	-> "Orange"
	| "Orange"	-> "Noir"
	| "Noir"	-> "Blanc"
	| "Blanc"	-> "Rouge";;

let rec changement_code position codeSelection =
	if position = 0 then
		(changement_couleur (List.hd codeSelection)) :: (List.tl codeSelection)
	else
		(List.hd codeSelection) :: (changement_code (position-1) (List.tl codeSelection));;

let selectionCouleur codeSelection col row =
	let e = Graphics.wait_next_event [Graphics.Button_down] in
	if e.Graphics.mouse_x > 250 && e.Graphics.mouse_x < 750 && e.Graphics.mouse_y > 50 && e.Graphics.mouse_y < (50 + (800/row)) then
		let position = map_selection e.Graphics.mouse_x col in changement_code position codeSelection
	else
		codeSelection;;


let drawGrille col row ecart =
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
		moveto (current_x () - 500) (current_y () + (fst ecart));
	done;
	moveto 250 50;
	for i = 0 to col do
		lineto (current_x () ) (current_y () + (fst ecart)); 
		moveto (current_x () + (fst ecart) ) (current_y () - (fst ecart));
	done;
	(*Bonnes reponses*)
	moveto 50 200;
	for i = 0 to row do
		lineto (current_x () + 100) (current_y ()); 
		moveto (current_x () - 100) (current_y () + (fst ecart));
	done;
	moveto 50 200;
	for i = 0 to 1 do
		lineto (current_x () ) (current_y () + 800); 
		moveto (current_x () + (fst ecart) ) (current_y() - 800);
	done;;

let draw_bonne_reponse reponse range row =
	moveto 100 (200 + (800 / row) * range + ((800 / row)/2));
	draw_string (string_of_int reponse)


let rec draw_all_code liste_code numero col row =
	drawCode (250 + ((500/col)/2)) (200 + ((800/row)/2) + ((800/row)*numero)) (snd (List.hd liste_code)) (500/col) ((min (500/col) (800/row))/2 - 2);
	draw_bonne_reponse (fst (List.hd liste_code)) numero row;
	if (List.tl liste_code) != [] then
		draw_all_code (List.tl liste_code) (numero + 1) col row;;

let rec ecranJeu col row listeCode codeSelection =
	clear_graph ();	
	drawGrille col row (500 / col , 800 / row);
	draw_all_code listeCode 0 col row;
	drawCode (250 + ((500/col)/2)) (50 + ((800/row)/2)) codeSelection (500/col) ((min (500/col) (800/row))/2 - 2);
	let codeSelection = selectionCouleur codeSelection col row in ecranJeu col row listeCode codeSelection;;

let attentInput () =
	let e = Graphics.wait_next_event [Graphics.Key_pressed] in
	if e.Graphics.keypressed then 
		let key = e.Graphics.key in
		if key = 'o' then ecranJeu 5 8 [(2,["Noir";"Noir";"Noir";"Noir";"Noir"]);(4,["Noir";"Noir";"Noir";"Noir";"Bleu"])] ["Noir";"Noir";"Noir";"Noir";"Noir"]
		else Graphics.close_graph ();;

let ecranDepart =
	moveto 450 500;
	set_text_size 100;
	draw_string "coucou t'es pret a jouer";
	moveto 450 450;
	draw_string "o pour oui n pour non";
	attentInput ();;

ecranDepart;;

(*
while true do
	let e = Graphics.wait_next_event [Graphics.Key_pressed] in
	if e.Graphics.keypressed then begin
	let key = e.Graphics.key in
	if key = '1' then Graphics.close_graph ()
	else if key = '2' then print_endline "2";
	end;
	print_endline "3";
done;; *)