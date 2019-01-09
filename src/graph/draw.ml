open Board;;

module Draw :
sig
        val init_draw_module :string -> unit
        val clearScreen : Sdlvideo.surface -> unit
        val drawImage : Sdlvideo.surface -> int -> int -> Sdlvideo.surface -> unit
          val draw_board_background : int -> Sdlvideo.surface -> unit
end = struct
        let board = Board.init_board;;
        
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

        let draw_board_background nombre_de_pion screen =
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
end;;
