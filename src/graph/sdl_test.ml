open Sdlevent;;

let logoMastermind = "logo.jpg";;
let logo = Sdlvideo.load_image logoMastermind;;
let fontMastermind = "font.ttf";;
let screenWidth = 600;;
let screenHeight = 800;;

let screen = Sdlvideo.set_video_mode screenWidth screenHeight [`DOUBLEBUF];;

let init_everything =
   Sdl.init [`VIDEO];
   Sdlwm.set_caption "Mastermind - Durand & Planchon" "";
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

let mousePosition mouseEvent =
  clearScreen screen;
  let x = mouseEvent.mme_x and y = mouseEvent.mme_y in
  drawImage logo x y screen;;

let rec wait_for_event () =
  let event = wait_event () in
    match event with
    | KEYDOWN {keysym=KEY_ESCAPE} ->
       print_endline "bye"
    | QUIT ->
       print_endline "bye"
    | MOUSEMOTION e ->
       mousePosition e;
       wait_for_event ();
    | e ->
        print_endline (string_of_event e);
        wait_for_event ()
        
let () =
  init_everything;
  
  drawImage logo 0 0 screen;
  drawImage logo 0 100 screen;
  Sdltimer.delay 1000; (* fade out *)
  clearScreen screen;
  wait_for_event();;
