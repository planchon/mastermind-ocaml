

module Board :
sig
        type board_background_images =
          {
                  score_gauche : Sdlvideo.surface;
                  score_milieu : Sdlvideo.surface;
                  score_impair : Sdlvideo.surface;
                  score_pair   : Sdlvideo.surface;
                  tick         : Sdlvideo.surface;
                  pion_gauche  : Sdlvideo.surface;
                  pion_milieu  : Sdlvideo.surface;
                  pion_droite  : Sdlvideo.surface;
          }

        type board_pion_images =
          {
                  rouge       : Sdlvideo.surface;
                  bleu        : Sdlvideo.surface;
                  cyan        : Sdlvideo.surface;
                  fonce       : Sdlvideo.surface;
                  grand_blanc : Sdlvideo.surface;
                  grand_noir  : Sdlvideo.surface;
                  jaune       : Sdlvideo.surface;
                  orange      : Sdlvideo.surface;
                  rose        : Sdlvideo.surface;
                  vert        : Sdlvideo.surface;
                  violet      : Sdlvideo.surface;
          }

        type board_score_image =
          {
                  petit_noir  : Sdlvideo.surface;
                  grand_noir  : Sdlvideo.surface;
                  petit_blanc : Sdlvideo.surface;
                  grand_blanc : Sdlvideo.surface;
          }

        type board_image =
          {
                  score : board_score_image;
                  pion  : board_pion_images;
                  bg    : board_background_images;
          }

        val init_board_images : board_background_images
        val init_pion_images  : board_pion_images
        val init_score_image  : board_score_image
        val init_board        : board_image

        val pion_of_couleur : string -> board_image -> Sdlvideo.surface
end = struct

        type board_background_images =
          {
                  score_gauche : Sdlvideo.surface;
                  score_milieu : Sdlvideo.surface;
                  score_impair : Sdlvideo.surface;
                  score_pair   : Sdlvideo.surface;
                  tick         : Sdlvideo.surface;
                  pion_gauche  : Sdlvideo.surface;
                  pion_milieu  : Sdlvideo.surface;
                  pion_droite  : Sdlvideo.surface;
          }

        type board_pion_images =
          {
                  rouge       : Sdlvideo.surface;
                  bleu        : Sdlvideo.surface;
                  cyan        : Sdlvideo.surface;
                  fonce       : Sdlvideo.surface;
                  grand_blanc : Sdlvideo.surface;
                  grand_noir  : Sdlvideo.surface;
                  jaune       : Sdlvideo.surface;
                  orange      : Sdlvideo.surface;
                  rose        : Sdlvideo.surface;
                  vert        : Sdlvideo.surface;
                  violet      : Sdlvideo.surface;
          }

        type board_score_image =
          {
                  petit_noir  : Sdlvideo.surface;
                  grand_noir  : Sdlvideo.surface;
                  petit_blanc : Sdlvideo.surface;
                  grand_blanc : Sdlvideo.surface;
          }

        type board_image =
          {
                  score : board_score_image;
                  pion  : board_pion_images;
                  bg    : board_background_images;
          }
        
        let init_board_images =
          {
                  score_gauche = Sdlloader.load_image "img/planche_gauche.png";
                  score_milieu = Sdlloader.load_image "img/planche_droite_score_milieu.png";
                  score_impair = Sdlloader.load_image "img/planche_droite_score_impair.png";
                  score_pair   = Sdlloader.load_image "img/planche_droite_score_pair.png";
                  tick         = Sdlloader.load_image "img/tick_ok.png";
                  pion_gauche  = Sdlloader.load_image "img/planche_pion_gauche.png";
                  pion_milieu  = Sdlloader.load_image "img/planche_milieu.png";
                  pion_droite  = Sdlloader.load_image "img/planche_droite.png";
          };;

        let init_pion_images =
          {
                  rouge       = Sdlloader.load_image "img/rouge.png";
                  bleu        = Sdlloader.load_image "img/bleu.png";
                  cyan        = Sdlloader.load_image "img/cyan.png";
                  fonce       = Sdlloader.load_image "img/fonce.png";
                  grand_blanc = Sdlloader.load_image "img/grand_blanc.png";
                  grand_noir  = Sdlloader.load_image "img/grand_noir.png";
                  jaune       = Sdlloader.load_image "img/jaune.png";
                  orange      = Sdlloader.load_image "img/orange.png";
                  rose        = Sdlloader.load_image "img/rose.png";
                  vert        = Sdlloader.load_image "img/vert.png";
                  violet      = Sdlloader.load_image "img/violet.png";     
          };;

        let init_score_image =
          {
                  petit_noir  = Sdlloader.load_image "img/noir.png";
                  grand_noir  = Sdlloader.load_image "img/grand_noir.png";
                  petit_blanc = Sdlloader.load_image "img/blanc.png";
                  grand_blanc = Sdlloader.load_image "img/grand_blanc.png";
          };;

        let init_board =
          {
                  score = init_score_image;
                  pion  = init_pion_images;
                  bg    = init_board_images;
          };;

        let pion_of_couleur col board =
          match col with
          | "Rouge"  -> board.pion.rouge;
          | "Bleu"   -> board.pion.bleu;
          | "Cyan"   -> board.pion.cyan;
          | "Fonce"  -> board.pion.fonce;
          | "Blanc"  -> board.pion.grand_blanc;
          | "Noir"   -> board.pion.grand_noir;
          | "Jaune"  -> board.pion.jaune;
          | "Orange" -> board.pion.orange;
          | "Rose"   -> board.pion.rose;
          | "Vert"   -> board.pion.vert;
          | "Violet" -> board.pion.violet;;                      
                      
end;;  
