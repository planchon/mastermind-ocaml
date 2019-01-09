open Sdlevent;;
open Draw;;
open Board;;

let nombre_de_pion = 11;;

let screenWidth =
  if nombre_de_pion mod 2 = 0 then
          (82 + 22 * ((nombre_de_pion - 4) / 2) + 47 + 81 + 52 * (nombre_de_pion - 2) + 182)
  else
          (82 + 22 * ((nombre_de_pion - 3) / 2) + 47 + 81 + 52 * (nombre_de_pion - 2) + 182);;
let screenHeight = 800;;

let screen = Sdlvideo.set_video_mode screenWidth screenHeight [`DOUBLEBUF];;

let () =
  Draw.init_draw_module "Mastermind - Paul & Thomas";
  Draw.draw_board_background nombre_de_pion screen;
  Sdltimer.delay 3000;;
  
