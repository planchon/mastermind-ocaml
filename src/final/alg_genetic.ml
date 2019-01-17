open Code;;

module Alg_genetic:
sig
        val play : Code.t  -> int
        val make_gen_move: (Code.t * (int * int)) list -> 'a -> Code.t
          
end = struct

        let max_pop_size                        = 60;;
        let max_generation                      = 100;;

        let crossover_probability               = 0.5;;
        let crossover_probability_then_mutation = 0.03;;
        let permutation_probability             = 0.03;;
        let inversion_probability               = 0.02;;

        let elite_ratio                         = 0.4;;

        let nombre_de_pion = int_of_string (Sys.argv.(1));;
        let nombre_de_couleurs = int_of_string (Sys.argv.(2));;

        let scores_ = Array.of_list ["Blanc"; "Noir"; "Null"];;
        
        let check_play ia_choice choice =
          Code.reponse ia_choice choice;;

        let crossover code1 code2 =
          let rec foo tmp i =
            if i >= 0 then
                    begin
                            if (Random.float 1.) > crossover_probability then
                                    foo (tmp @ [(List.nth code1 i)]) (i - 1)
                            else
                                    foo (tmp @ [(List.nth code2 i)]) (i - 1)
                    end
            else
                    tmp
          in foo [] (nombre_de_pion - 1);;
        
        (* tous les codes relatifs à la genetic *)
        let permute code =
          let a_code = Array.of_list code in
          let rec foo tmp i =
            if (i > 0) then
                    begin
                            if ((Random.float 1.) <= permutation_probability) then
                                    begin
                                            let color_a_position = (Random.int (nombre_de_pion - 1)) in
                                            let color_b_position = (Random.int (nombre_de_pion - 1)) in
                                            let saved_color_a = (Array.get tmp color_a_position) in
                                            
                                            (Array.set tmp color_a_position (Array.get tmp color_b_position));
                                            (Array.set tmp color_b_position saved_color_a);
                                            foo tmp (i - 1);
                                    end
                            else
                                    begin
                                            foo tmp (i - 1);
                                    end
                    end
            else
                    Array.to_list tmp;
          in foo a_code nombre_de_pion;;

        let mutate code =
          let a_code = Array.of_list code in
          let i = Random.int (nombre_de_pion - 1) in
          let c = (List.nth Code.couleur_possibles (Random.int ((List.length Code.couleur_possibles) - 1))) in
          Array.set a_code i c;
          Array.to_list a_code;;

        let generate_dummy_population pop_size =
          if pop_size > ((List.length Code.tous) - 100) then
                          let rec foo tmp i =
                            if (i > 0) then
                                    let code = (List.nth Code.tous (Random.int (List.length Code.tous))) in
                                    if List.exists (fun x -> (code = x)) tmp then
                                            begin
                                                    foo (tmp) (i);
                                            end
                                    else
                                            begin
                                                    foo (tmp @ [code]) (i - 1);
                                            end
                            else
                                    tmp
                            in foo [] pop_size
          else
                  Code.tous;;

        let generate_son_population population =
          let rec foo tmp i =
            if (i > 1) then
                    begin
                            if ((Random.float 1.) < crossover_probability_then_mutation) then
                                    begin
                                            let son = crossover (List.nth population (i - 2)) (List.nth population (i - 1)) in
                                            foo (tmp @ [(permute(mutate(son)))]) (i - 1);
                                    end
                            else
                                    begin
                                            let son =  mutate (crossover (List.nth population (i - 2)) (List.nth population (i - 1))) in
                                            foo (tmp @ [(permute(son))]) (i - 1);
                                    end
                    end
            else
                    tmp
          in foo [] (List.length population);;

        let fitness trial guesses =
          let rec foo guess tmp =
            match guess with
            | h :: t ->
               begin
                       let res = (check_play trial (fst h)) in
                       let code_h = (snd h) in
                       foo t (tmp + abs((fst res) - (fst code_h)) + abs((snd res) - (snd code_h)));
               end
            | _ -> tmp
          in foo guesses 0;;
        
        let generate_pop_score son costfunction =
          let rec foo tmp son =
            match son with
            | code :: t ->
               begin
                       foo (tmp @ [((costfunction code), code)]) t;
               end
            | _ -> tmp
          in foo [] son;;

        let generate_new_chosen_ones ele cho pop_size =
          let rec remplace_doublons tmp_ele tmp_cho =
            match tmp_ele with
            | h :: t ->
               begin
                       if List.exists (fun x -> x = h) cho then
                               begin
                                       let new_code = List.nth Code.tous (Random.int (List.length Code.tous)) in
                                       remplace_doublons t (tmp_cho @ [new_code]);
                               end
                       else
                               remplace_doublons t (tmp_cho @ [h])
               end
            | _ -> tmp_cho
          in remplace_doublons ele cho;;

        let fill_population population pop_size =
          let rec foo tmp =
            if (List.length tmp) < pop_size then
                    foo (tmp @ [(List.nth Code.tous (Random.int (List.length Code.tous)))])
            else
                    tmp
          in foo population;;
        
        let genetic_evolution pop_size generation costfunction =
          let population = (generate_dummy_population pop_size) in
          
          let rec generate choosen_ones pop h =
            if ((List.length choosen_ones) <= pop_size) && (h <= generation) then
                    begin
                            (* on fait muter les parents pour faire des enfants, d'apres la selection naturelle *)
                            let sons      = (generate_son_population pop) in
                            
                            (* on trouve le score de chaque code en fonction des anciens scores (le plus proche de 0 le mieux) *)
                            let pop_score = (generate_pop_score sons costfunction) in

                            (* on ne garde que les codes elite, cad de score = 0 *)
                            let elegible  = (List.fold_right (fun e liste -> if (fst e) = 0 then liste @ [snd e] else liste) pop_score []) in

                            (* on ajoute aux elites les codes eligibles, si le code est deja dedans, on prend un code au hasard, de cette facon la diversité genetique est respectee *)
                            let new_chosen_ones = (generate_new_chosen_ones elegible choosen_ones pop_size) in
                            (* la prochaine population (les parents) sont les anciennes elites, plus des codes aleatoires pour rendre la taille de la population fixes *)
                            let new_population  = (fill_population new_chosen_ones pop_size) in

                            generate new_chosen_ones new_population (h + 1);
                    end
            else
                    begin
                            choosen_ones;
                    end
          in generate [] population 1;;

        let rec do_not_return_until_not_empty func max_pop max_gen =
          let liste = (func max_pop max_gen) in 
          if (List.length liste) = 0 then
                  begin     
                          do_not_return_until_not_empty func (max_pop * 2) (max_gen / 2)
                  end
          else
                  liste;;


        let rec find_code ele guess =
          match ele with
          | h :: t when (List.exists (fun x -> x = h) guess) = false -> h
          | h :: t -> find_code t guess
          | _ -> [];;

        let rec print_code code =
          match code with
          | h :: t ->
             begin
                     print_string (h ^ " ");
                     print_code t;
             end    
          | _ -> print_endline "";;

        let rec make_gen_move guess last_code =   
          let elegible = (do_not_return_until_not_empty (fun x y -> genetic_evolution x y (fun a -> fitness a guess)) max_pop_size max_generation) in
          find_code elegible (List.fold_right (fun x liste -> liste @ [(fst x)]) guess []);;

        let play secret =
          let do_play = (fun x -> check_play x secret) in
          let code_depart = ["Rouge"; "Rouge"; "Vert"; "Vert"] in
          let result = do_play code_depart in
          let guess = [(code_depart, result)] in

          let rec genetic result guess code =
            if ((fst result) != 4) && ((List.length guess) < 10) then
                    begin
                            let elegible = (do_not_return_until_not_empty (fun x y -> genetic_evolution x y (fun a -> fitness a guess)) max_pop_size max_generation) in
                            let code_to_play = find_code elegible (List.fold_right (fun x liste -> liste @ [(fst x)]) guess []) in
                            genetic (do_play code_to_play) (guess @ [(code_to_play, (do_play code_to_play))]) code_to_play;
                    end
            else
                    begin
                            let essais = List.length guess in
                            if essais > 9 then
                                    0
                            else
                                    1
                    end
          in genetic result guess code_depart;;
end;;
