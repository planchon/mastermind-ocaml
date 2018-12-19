let clearScreen screen =
  Sdlvideo.fill_rect screen (Sdlvideo.map_RGB screen Sdlvideo.white);
  Sdlvideo.flip screen;;

let initEverything () =
        Sdl.init [`VIDEO];
        Sdlwm.set_caption "Mastermind - Durand & Planchon" "";
        at_exit Sdl.quit;
        Sdlttf.init ();
        at_exit Sdlttf.quit;;

let main () = 
        initEverything ();
        let nombreDePions = 4 and nombreDeCoups = 10 in
        let board = Board.newBoard nombreDePions nombreDeCoups in
        let rec jouer vie =
                clearScreen board.screen;
                Board.drawBoard board;
                Sdltimer.delay 5000;
        in jouer 10;;

let () = main();;