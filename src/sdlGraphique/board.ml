type backGroundImageFolder = {
    hautGauche : Sdlvideo.surface;
    hautDroit : Sdlvideo.surface;
    hautMilieu : Sdlvideo.surface;
    basMilieu : Sdlvideo.surface;
    basDroit : Sdlvideo.surface;
    basGauche : Sdlvideo.surface;
    coteDroit : Sdlvideo.surface;
    coteGauche : Sdlvideo.surface;
}

type listePion = {
    rouge: Sdlvideo.surface;
    orange: Sdlvideo.surface;
    vert: Sdlvideo.surface;
    bleu: Sdlvideo.surface;
    noir: Sdlvideo.surface;
    blanc: Sdlvideo.surface;
}

type boardType = {
    nombrePions: int;
    nombreCous : int;
    tailleTileContour : int;
    tailleTilePion : int;
    coupsJoues : (string list) list;
    scoreJoues : (int * int) list;
    grilleImage : backGroundImageFolder;
    pions : listePion;
    screen     : Sdlvideo.surface;
}

let newBoard pions coups = {
    nombrePions = pions;
    nombreCous  = coups;
    tailleTileContour  = 25;
    tailleTilePion = 17;
    coupsJoues  = [["Vert"; "Bleu"; "Noir"; "Bleu"]; ["Vert"; "Rouge"; "Blanc"; "Bleu"]];
    scoreJoues  = [(1,2);(2,2)]; 
    grilleImage = {
        hautGauche = Sdlloader.load_image "images/board/hautGauche.jpg";
        hautDroit = Sdlloader.load_image "images/board/hautDroit.jpg";
        hautMilieu = Sdlloader.load_image "images/board/hautMilieu.jpg";
        basMilieu = Sdlloader.load_image "images/board/basMilieu.jpg";
        basDroit = Sdlloader.load_image "images/board/basDroit.jpg";
        basGauche = Sdlloader.load_image "images/board/basGauche.jpg";
        coteDroit = Sdlloader.load_image "images/board/coteDroit.jpg";
        coteGauche = Sdlloader.load_image "images/board/coteGauche.jpg";
    };
    pions = {
        noir = Sdlloader.load_image "images/pions/noir.jpg";
        rouge = Sdlloader.load_image "images/pions/noir.jpg";
        orange = Sdlloader.load_image "images/pions/noir.jpg";
        vert = Sdlloader.load_image "images/pions/noir.jpg";
        bleu = Sdlloader.load_image "images/pions/noir.jpg";
        blanc = Sdlloader.load_image "images/pions/noir.jpg";
    };
    screen = Sdlvideo.set_video_mode 600 800 [`DOUBLEBUF];
};;

let drawImage image x y screen =
  let position_of_image = Sdlvideo.rect x y 0 0 in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:screen ();
  Sdlvideo.flip screen;;

let drawRectangle board x y w h = 
    drawImage board.grilleImage.hautGauche x y board.screen;
    drawImage board.grilleImage.hautDroit (x + w * board.tailleTileContour) (y) board.screen;
    drawImage board.grilleImage.basDroit (x + w * board.tailleTileContour) (y + h * board.tailleTileContour) board.screen;
    drawImage board.grilleImage.basGauche (x) (y + h * board.tailleTileContour) board.screen;
    for i=1 to w-1 do 
        drawImage board.grilleImage.hautMilieu (x + i * board.tailleTileContour) (y) board.screen;
        drawImage board.grilleImage.basMilieu (x + i * board.tailleTileContour) (y + h * board.tailleTileContour) board.screen;
    done;
    for i=1 to h-1 do 
        drawImage board.grilleImage.coteDroit (x + w * board.tailleTileContour) (y + i * board.tailleTileContour) board.screen;
        drawImage board.grilleImage.coteGauche (x) (y + i * board.tailleTileContour) board.screen;
    done;;

let choosePion pion board = 
    match pion with
    | "Rouge" -> board.pions.rouge;
    | "Vert" -> board.pions.vert;
    | "Blanc" -> board.pions.blanc;
    | "Bleu" -> board.pions.bleu;
    | "Orange" -> board.pions.orange;
    | "Noir" -> board.pions.noir;;

let drawBoard board = 
    drawRectangle board 25 25 (2 + 1 + board.nombrePions) 1;
    drawRectangle board 125 100 6 10;
    drawRectangle board 25 100 2 10;
    for i=1 to ((List.length board.coupsJoues) - 1) do
        for j=1 to board.nombrePions - 1 do
            drawImage (choosePion (List.nth (List.nth board.coupsJoues i) j) board) (j * board.tailleTilePion) (i * board.tailleTileContour) board.screen;
        done;
    done;;
