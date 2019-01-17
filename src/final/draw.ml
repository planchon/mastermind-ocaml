open Board;;
open Sdlevent;;
open Sdlvideo;;

module Draw :
sig
        val init_draw_module : string  -> unit
        val clearScreen : Sdlvideo.surface -> unit
        val drawImage : Sdlvideo.surface -> int -> int -> Sdlvideo.surface -> unit
        val draw_board_background : unit -> unit
        val mouse_event : mousebutton_event -> int
        val draw_interactive_pion : string list -> unit
        val draw_pions : string list list -> unit
        val draw_score : (int * int) list -> unit
        val render_text : string -> int -> int -> unit
        val get_text : int -> int -> string
        val render_text_center : string -> unit
        val render_text_center_y : string -> int -> unit
        val menu_type_de_partie : string -> string list -> int

        val screenWidth : int  
        val screenHeight : int
        val screen : Sdlvideo.surface
end = struct
        let nombre_de_pion = int_of_string (Sys.argv.(1));;

        (** 
        *  @return la taille en largeur de l'ecran en fonction du nombre de pions
        *)
        let screenWidth =
          if nombre_de_pion mod 2 = 0 then
                  (82 + 22 * ((nombre_de_pion - 4) / 2) + 47 + 81 + 52 * (nombre_de_pion - 2) + 182)
          else
                  (82 + 22 * ((nombre_de_pion - 3) / 2) + 47 + 81 + 52 * (nombre_de_pion - 2) + 182);;

        let screenHeight = 800;;
        
        let board = Board.init_board;;
        let screen = Sdlvideo.set_video_mode screenWidth screenHeight [`DOUBLEBUF];;

        let const_pion_start =
          if nombre_de_pion mod 2 = 0 then
                  ((((82 + 22 * ((nombre_de_pion - 4) / 2)) + 47)))
          else
                  ((((82 + 22 * ((nombre_de_pion - 3) / 2)) + 47)));;
        
        let init_draw_module title=
          Sdl.init [`VIDEO];
          Sdlttf.init ();
          Sdlwm.set_caption title "";
          at_exit Sdl.quit;
          at_exit Sdlttf.quit;;
        
        (** Affiche une image sur le screen
        *  @param image l'image
        *  @param x y la position de l'image
        *  @param screen la surface
        *  @return unit
        *)
        let drawImage image x y screen =
          let position_of_image = Sdlvideo.rect x y 0 0 in
          Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
          Sdlvideo.flip screen;;

        (** clear le screen
        *  @param screen la surface
        *  @return unit
        *)
        let clearScreen screen =
          Sdlvideo.fill_rect screen (Sdlvideo.map_RGB screen Sdlvideo.black);
          Sdlvideo.flip screen;;
        
        (** retourne la position de la souris (un entier entre 0 et 20 environs)
        *  @param e event de la souris
        *  @return unit
        *)
        let mouse_event e =
          (* on est dans la bonne zone horizontale *)
          let x = (e.mbe_x) and y = e.mbe_y in
          if (y > 711) && (y < 764) && (x > 50) && (x < const_pion_start - 14) then
                  0
          else
                  if (y > 726) && (y < 750) then
                          ((x - const_pion_start) + 10) / 52
                  else
                          -1;;
        
        (** affiche du text a lecran
        *  @param text le texte
        *  @param x y la position
        *  @return unit
        *)
        let render_text text x y =
          let font = Sdlttf.open_font "font/font.ttf" 34 in
          let text_rendered = Sdlttf.render_text_blended font text ~fg:Sdlvideo.white in
          let pos = Sdlvideo.rect x y 0 0 in
          let rectangle = Sdlvideo.rect pos.r_x pos.r_y (Sdlvideo.surface_info text_rendered).w (Sdlvideo.surface_info text_rendered).h in
          
          Sdlvideo.fill_rect ~rect:rectangle screen Int32.zero;
          Sdlvideo.blit_surface ~dst_rect:pos ~src:text_rendered ~dst:screen ();
          Sdlvideo.flip screen;;
        
        (** permet de pouvoir ecrire sur lecran
        *  @param x y la position de la zone d'ecriture
        *  @return le texte ecrit
        *)
        let get_text x y =
          let rec foo text =
            if (String.compare text "") != 0 then
                    render_text text x y;
            let event = wait_event() in
            match event with
            | KEYDOWN {keysym=KEY_ESCAPE} ->
               print_endline "Merci d'avoir joué <3";
               exit 0;
            | QUIT ->
               print_endline "Merci d'avoir joué <3";
               exit 0;
            | KEYDOWN {keysym = KEY_RETURN} ->
               text;
            | KEYDOWN e when (((Sdlkey.int_of_key e.keysym) > 31) && ((Sdlkey.int_of_key e.keysym) < 123)) ->
               foo (text ^ (String.make 1 (Sdlkey.char_of_key e.keysym)));
            | _ ->
               foo text;
          in foo "";;

        (** fais un rendu de texte centré
        *  @param text le texte
        *  @return unit
        *)
        let render_text_center text =
          (* on fait le rendu du gros text chiant *)
          let font = Sdlttf.open_font "font/font.ttf" 34 in
          let text_rendered = Sdlttf.render_text_blended font text ~fg:Sdlvideo.white in
          let pos = Sdlvideo.rect (screenWidth / 2 - (Sdlvideo.surface_info text_rendered).w / 2) (screenHeight / 2 - (Sdlvideo.surface_info text_rendered).h / 2) 0 0 in
          let rectangle = Sdlvideo.rect pos.r_x pos.r_y (Sdlvideo.surface_info text_rendered).w (Sdlvideo.surface_info text_rendered).h in
          
          Sdlvideo.fill_rect ~rect:rectangle screen Int32.zero;
          Sdlvideo.blit_surface ~dst_rect:pos ~src:text_rendered ~dst:screen ();
          Sdlvideo.flip screen;;      

        (** fais un rendu de texte aligné en x mais selon une valeur de y
        *  @param text le texte
        *  @param y la position de l'image
        *  @return unit
        *)
        let render_text_center_y text y =
          (* on fait le rendu du gros text chiant *)
          let font = Sdlttf.open_font "font/font.ttf" 34 in
          let text_rendered = Sdlttf.render_text_blended font text ~fg:Sdlvideo.white in
          let pos = Sdlvideo.rect (screenWidth / 2 - (Sdlvideo.surface_info text_rendered).w / 2) y 0 0 in
          let rectangle = Sdlvideo.rect pos.r_x pos.r_y (Sdlvideo.surface_info text_rendered).w (Sdlvideo.surface_info text_rendered).h in
          
          Sdlvideo.fill_rect ~rect:rectangle screen Int32.zero;
          Sdlvideo.blit_surface ~dst_rect:pos ~src:text_rendered ~dst:screen ();
          Sdlvideo.flip screen;;      

        (** permet d'afficher un menu et retourner le boutton sur lequel l'user a clique
        *  @param text le titre du menu
        *  @param option les differentes options proposée (3 max)
        *  @return le numero de la reponse
        *)
        let menu_type_de_partie text option =
          render_text_center text;
          render_text (List.nth option 0) (screenWidth / 2 - 200) (screenHeight / 2 + 15);
          render_text (List.nth option 1) (screenWidth / 2 - 50) (screenHeight / 2 + 15);
          render_text (List.nth option 2) (screenWidth / 2 + 90) (screenHeight / 2 + 15);
          let rec foo () =
            let event = wait_event() in
            match event with
            | KEYDOWN {keysym=KEY_ESCAPE} ->
               print_endline "Merci d'avoir joué <3";
               exit 0;
            | QUIT ->
               print_endline "Merci d'avoir joué <3";
               exit 0;
            | MOUSEBUTTONDOWN e ->
               begin
                       match e.mbe_x with
                       | a when a < (screenWidth / 2 - 100) -> 0;
                       | a when a > (screenWidth / 2 + 100) -> 2;
                       | _ -> 1;
               end
            | _ ->
               foo ();
          in foo ();;
        
        (** affiche le background
        *  @return unit
        *)
        let draw_board_background () =
          drawImage board.bg.score_gauche 0 0 screen;
          if nombre_de_pion mod 2 = 0 then
                  begin
                          for i = 0 to ((nombre_de_pion - 4) / 2) - 1 do
                                  drawImage board.bg.score_milieu (82 + 22 * i) 0 screen;
                          done;
                          drawImage board.bg.score_pair (82 + 22 * ((nombre_de_pion - 4) / 2)) 0 screen;
                          drawImage board.bg.pion_gauche ((82 + 22 * ((nombre_de_pion - 4) / 2)) + 47) 0 screen;
                          for i = 0 to (nombre_de_pion - 2) do
                                  drawImage board.bg.pion_milieu (((82 + 22 * ((nombre_de_pion - 4) / 2)) + 47) + 81 + i * 52) 0 screen;
                          done;
                          drawImage board.bg.pion_droite (((82 + 22 * ((nombre_de_pion - 4) / 2)) + 81 + 47) + (nombre_de_pion - 2) * 52) 0 screen;
                  end
          else
                  begin
                          for i = 0 to ((nombre_de_pion - 3) / 2) do
                                  drawImage board.bg.score_milieu (82 + 22 * i) 0 screen;
                          done;
                          drawImage board.bg.score_impair (82 + 22 * ((nombre_de_pion - 3) / 2)) 0 screen;
                          drawImage board.bg.pion_gauche ((82 + 22 * ((nombre_de_pion - 3) / 2)) + 47) 0 screen;
                          for i = 0 to (nombre_de_pion - 2) do
                                  drawImage board.bg.pion_milieu (((82 + 22 * ((nombre_de_pion - 3) / 2)) + 47) + 81 + i * 52) 0 screen;
                          done;
                          drawImage board.bg.pion_droite (((82 + 22 * ((nombre_de_pion - 3) / 2)) + 47) + 81 + (nombre_de_pion - 2) * 52) 0 screen;
                  end;
          drawImage board.bg.tick ((50 + 81 + 22 * ((nombre_de_pion - 4) / 2) + 31) / 2 - (10 * ((nombre_de_pion + 1) mod 2)) - 5)  719 screen;;
        
        (** Affiche un code selon une position en y
        *  @param pions le code
        *  @param y la position
        *  @return unit
        *)
        let draw_pion_liste pions y =
          let pions = Array.of_list pions in
          for i = 0 to (Array.length pions) - 1 do
                  drawImage (Board.pion_of_couleur (Array.get pions i) board) (const_pion_start + 42 + i * 52) y screen;
          done;;

        (** affiche un score
        *  @param score le score
        *  @param y la position
        *  @return unit
        *)
        let draw_one_score score y =
          (* pions noir *)
          let noir = (board.score.petit_noir) and blanc = (board.score.petit_blanc) in
          for i=0 to (fst score) - 1 do
                  if i mod 2 = 0 then
                          drawImage noir (66 + (i / 2) * 22) y screen
                  else
                          drawImage noir (66 + (i / 2) * 22) (y + 18) screen
          done;
          for i= (fst score) to (snd score) + (fst score) - 1 do
                  if i mod 2 = 0 then
                          drawImage blanc (66 + (i / 2) * 22) y screen
                  else
                          drawImage blanc (66 + (i / 2) * 22) (y + 18) screen
          done;;
                
        (** affiche tous les scores
        *  @param liste_scores tous les scores
        *  @return unit
        *)
        let draw_score liste_score =
          for i= 0 to (List.length liste_score) - 1 do
                  draw_one_score (List.nth (List.rev liste_score) i) (656 - i * (52 + 15));
          done;;
        
        (** affiche tous les pions
        *  @param liste_pions les pions a afficher
        *  @return unit
        *)
        let draw_pions liste_pions =
          for i= 1 to (List.length liste_pions) do
                  draw_pion_liste (List.nth (List.rev liste_pions) (i - 1)) (726 - 67 * i)
          done;;
       
        (** affiche les pions dans la partie touchable
        *  @param pions les pions a afficher
        *  @return unit
        *)
        let draw_interactive_pion pions =
          draw_pion_liste pions 726;;
end;;
