open Board;;
open Sdlevent;;

module Draw :
sig
        val init_draw_module :string -> unit
        val clearScreen : Sdlvideo.surface -> unit
        val drawImage : Sdlvideo.surface -> int -> int -> Sdlvideo.surface -> unit
        val draw_board_background : unit
        val mouse_event : mousebutton_event -> int
        val draw_interactive_pion : string list -> unit
        val draw_pions : string list list -> unit
        val draw_score : (int * int) list -> unit
            
end = struct
        let nombre_de_pion = 4;;

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
        
        let init_draw_module title =
          Sdl.init [`VIDEO];
          Sdlwm.set_caption title "";
          at_exit Sdl.quit;
          Sdlttf.init ();
          at_exit Sdlttf.quit;;

        let drawImage image x y screen =
          let position_of_image = Sdlvideo.rect x y 0 0 in
          Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
          Sdlvideo.flip screen;;

        let clearScreen screen =
          Sdlvideo.fill_rect screen (Sdlvideo.map_RGB screen Sdlvideo.black);
          Sdlvideo.flip screen;;
        
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

        let draw_board_background =
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
                  end;;
        
        (* prend une liste de pion de type pion et retourne la couleur et les affiche *) 
        let draw_pion_liste pions y =
          let pions = Array.of_list pions in
          for i = 0 to (Array.length pions) - 1 do
                  drawImage (Board.pion_of_couleur (Array.get pions i) board) (const_pion_start + 42 + i * 52) y screen;
          done;;

        let draw_one_score score y =
          (* pions noir *)
          let noir = (Board.score.petit_noir) and blanc = (Board.score.petit_blanc) in
          for i=0 to (fst score) do
                  if i mod 2 = 0 then
                          drawImage noir 50 (y + 50); 
                  else
                          drawImage noir 50 y;
          done;
          for i=(fst score) to (snd score) do
                  if i mod 2 = 0 then
                          drawImage blanc 50 (y + 50); 
                  else
                          drawImage blanc 50 y;
          done;;
                
        let draw_score liste_score =
          for i=1 to (List.length liste_score) do
                  draw_one_score (List.nth liste_score (i - 1)) 726;
          done;;
        
        let draw_pions liste_pions =
          for i=1 to (List.length liste_pions) do
                  match i with
                  | 1 -> draw_pion_liste (List.nth liste_pions (i - 1)) (726 - 69)
                  | 3 -> draw_pion_liste (List.nth liste_pions (i - 1)) (726 - 67 * 3 + 1)
                  | 7 -> draw_pion_liste (List.nth liste_pions (i - 1)) (726 - 67 * 7 + 1)
                  | 8 -> draw_pion_liste (List.nth liste_pions (i - 1)) (726 - 67 * 8 + 1)
                  | 9 -> draw_pion_liste (List.nth liste_pions (i - 1)) (726 - 67 * 9 + 2)
                  | 10 -> draw_pion_liste (List.nth liste_pions (i - 1)) (726 - 67 * 10 + 2)
                  | _ ->
                     draw_pion_liste (List.nth liste_pions (i - 1)) (726 - 67 * i)
          done;;
        
        let draw_interactive_pion pions =
          draw_pion_liste pions 726;;
end;;
