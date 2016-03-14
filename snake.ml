(*
   Jeu Snake (squelette de programme)
   
   compilation: ocamlc -o snake unix.cma graphics.cma snake.ml
*)
open Graphics
  (* utiliser la librairie Graphics pour toutes les
     fonctions d'affichage et d'interaction *)

let width = 800 (* largeur du plateau *)
let height = 600 (* hauteur du plateau *)


(* 
  Type permetant d'identifier un joueur par rapport à son nom, la couleur de son serpent,
   ainsi que de ces touches pour se déplacer 
*)
type joueur ={
  nom : string;
  couleurSerp : color;
  haut : char;
  bas : char;
  gauche : char;
  droite : char;
}


type objCase = Obstacle (* obstacle que le serpent peut rencontrer,
              identique au bord excepter la couleur *) 
  | Bord (* bord du terrain *)
  | Pastille  (* pastille que le serpent peut manger*)
  | Serpent of joueur (* case contenant une partie d'un serpent, Joueur permet de distinger la couleur (ex: J1 = case bleu) *)
  | Vide (* case vide *)


(* une case est définit par ces coordonnées en x et en y -> (x,y)
    ainsi que de son contenu (ex: bord du terain, serpent, pastille ... )*)
type case = {
  coord : (int*int); 
  contenant : objCase;
}



(* un serpet est composé de : 
  dir correspond à la direction vers lequel se déplace le serpent
  
  composition est la liste des case ou se trouve le corp du serpent
    la tête est forcément le 1er element et le bout de sa queue le dernier element

    score associer au serpent

    appartient permet d'identifier à quel joueur appartient le serpent
*)
type orientation = Nord | Sud | Est | Ouest
type serpent = {
  dir : orientation;
  composition : (int*int) list;
  score : int; (* score de base : 0 *)
  appartient : joueur;
}

(* l'état courant d'un jeux est composé:
  d'une list de case
  listeSerp correspond à la liste des serpents sur le jeux.

  Il est préférable de garder le même ordre tout le long de 
  la partie pour évité quelque problème lors de l'affichage des scores.
  (l'affichage des scores s'affiche dans l'ordre de la liste, si l'ordre change 
  le texte sera illisible car le texte changera tout le temps ) 
*)
type state = {
  damier : case list;
  listeSerp : serpent list;
}

(* constante *)
let tCase = 13 (* longueur d'une case en pixel, utilisé pour dessiner les cases *)

(* Nombre de case dans le damier: hDamier*lDamier *)
let hDamier = height/tCase (* damier de même hauteur que la fenètre  *)
let lDamier = width/(tCase+2) (* longueur du damier prend presque toute la longueur de la fenètre.
                                le +2 sert à laisser de l'espace à droite pour le score *)

(* nombre de pastille à récupérer à chaque étape et qui fait accéléré le niveau *)
let nbrPastille = 3
(* It_interval de base *)
let itInterval = 0.1


(* serpent du joueur 1 de base lors du lancement du jeu *)
let j1 = {nom = "Joueur 1"; couleurSerp = blue; 
  haut = 'z'; bas = 's'; gauche = 'q'; droite = 'd'}

let serpJ1 = {dir=Est ; 
  composition = [(4,hDamier/2); (3,hDamier/2);(2,hDamier/2)] ; 
  score = 0; appartient = j1}

(* serpent du joueur 2 de base lors du lancement du jeu *)
let j2 = {nom = "Joueur 2"; couleurSerp = magenta;
  haut = 'u'; bas = 'j'; gauche = 'h'; droite = 'k'}
let serpJ2 = {dir=Ouest ;
  composition = [(lDamier-5,hDamier/2);(lDamier-4,hDamier/2);(lDamier-3,hDamier/2)] ; 
  score = 0; appartient = j2}

(* liste des serpent en jeu. Enlever serpJ2 pour jouer en 1 joueur *)
let listeSerpIni = [serpJ1;serpJ2]

(* liste des obstacles *)
let obstacle =  [(* bloc en haut à gauche *)
                 (3,hDamier-4);(3,hDamier-5);
                 (4,hDamier-4);(4,hDamier-5);
                 (* bloc en bas à gauche *)
                 (3,3);(3,4);
                 (4,3);(4,4);
                 (* bloc en bas à droite *)
                 (lDamier-4,3);(lDamier-4,4);
                 (lDamier-5,3);(lDamier-5,4);
                (* bloc en haut à droite *)
                 (lDamier-4,hDamier-4);(lDamier-4,hDamier-5);
                 (lDamier-5,hDamier-4);(lDamier-5,hDamier-5);                   
                (* bloc milieu *)
                 (lDamier/2,hDamier/2);(lDamier/2-1,hDamier/2);
                 (lDamier/2,hDamier/2-1);(lDamier/2-1,hDamier/2-1);  
                ]

(* 
  s : un serpent
  damier : liste des cases à metre à jours
  
  Renvoi le damier modifier avec le serpent incruster.
  si une case contient un serpent mais que les coordonnées 
  de cette case ne se trouve pas dans s, alors le contenue
  de la case est mis à vide.

  Ne vérifie pas que le serpent se trouve sur un bord ou un obstacle
*)
let rec insererSerpent s damier = match s.composition,damier with
  _ ,[] -> damier 
  | [], _ -> damier
  | listCoupleInt,e::ds -> (match e.coord with 
      (ex,ey) -> 
        (if (List.exists (fun (x,y) -> x=ex && y=ey ) listCoupleInt) 
            then {e with contenant = Serpent s.appartient}
          else if e.contenant = Serpent s.appartient then {e with contenant = Vide}
          else e
        )::(insererSerpent s ds)
      )

(* Applique insererSerpent depuis une liste de serpent sur un damier *)
let insererPlusieurSerp lSerp damier = List.fold_left (fun acc serp -> insererSerpent serp acc) damier lSerp


(* Modifie le contenant d'une case de coordonnée (x,y) dans le damier *)
let rec changerCase (x,y) contenuAMetre damier = match damier with
  | [] -> failwith "Erreur lors de l'appel à changerCase, cette case n'existe pas"
  | e::s -> if e.coord = (x,y) then {e with contenant = contenuAMetre}::s
            else e::(changerCase (x,y) contenuAMetre s)

(* Prend une liste de coordonnée et remplis chaque case avec ces coordonées avec un obstacle*)
let ajouterObstacle obs damier = List.fold_left (fun acc couple -> changerCase couple Obstacle acc) damier obs



(* 
  damier : liste de case
  contenu : type de case que l'on souhaite compter
  renvoi le nombre de case ayant case.contenant = contenu *)
let rec nbrCase contenu damier = List.fold_left (fun acc case -> 
                                              (if case.contenant = contenu 
                                               then acc+1 else acc)
                                            ) 
                            0 damier

(* Remplace un case vide numeros 'indice' du damier par une case contenant 'typeCase' *)
let rec ajouterContenantCase typeCase indice damier = match damier with
  [] -> failwith "Plus de place, partie terminé"
  | e::s -> if e.contenant = Vide 
              then if indice = 0 then {e with contenant = typeCase}::s
                else e::(ajouterContenantCase typeCase (indice-1) s)
            else e::(ajouterContenantCase typeCase indice s)

(* 
  Place une case contenant 'typeCase' aléatoirement sur le damier dans une case vide.
  s'il n'y à plus de case vide, la partie est fini
*)
let rec genererContenantCase typeCase damier = match damier with
  [] -> failwith "Erreur, damier vide"
  | e::s -> let nbrCVide = ((nbrCase Vide (damier))) in
              if nbrCVide = 0 then failwith "Plus de place, partie terminé"
              else ajouterContenantCase typeCase ((Random.int nbrCVide)-1) damier


(* Génere un damier vide de taille l*h *)
let rec auxGenCases l h lAct hAct listeAcc = 
  if l > lAct && h > hAct then 
    (auxGenCases l h (lAct+1) hAct ({coord = (lAct,hAct); contenant = Vide}::listeAcc))
  else if h > hAct then
    (auxGenCases l h 0 (hAct+1) listeAcc)
  else listeAcc

let genCases l h = auxGenCases l h 0 0 []

(* Génère des cases remplie de "Bord" tout autour du damier *)
let rec genererBord damier = match damier with
  [] -> damier
  | e::s -> (match e.coord with
              x,y -> if x = 0 || y = 0 || x = (lDamier-1) || y = (hDamier-1)
                      then {e with contenant = Bord}::(genererBord s)
                    else e::genererBord s)

(* Ajoute un couple d'int dans la liste qui compose un serpent *)
let avancer s case = {s with composition = case.coord::s.composition}
  

(* Recupère la case de coordoner (x,y) *)
let rec recupCase x y damier = match damier with
  [] -> failwith "Erreur, cette case n'existe pas"
  | e::s -> match e.coord with 
            (ex,ey) -> if x = ex && y = ey 
                        then e
                        else recupCase x y s

(* Renvoi la case ou devra etre le serpent après avoir avancer *)
let recupProchaineCase s damier = match s.composition,s.dir with
  [],_ -> failwith "Erreur, le serpent n'a pas de tête"  
  | ((x,y)::_),Nord -> recupCase x (y+1) damier
  | ((x,y)::_),Sud -> recupCase x (y-1) damier
  | ((x,y)::_),Est -> recupCase (x+1) y damier
  | ((x,y)::_),Ouest -> recupCase (x-1) y damier



(* Supprime le dernier element d'une liste *)
let rec supDernier l = match l with
  [] -> failwith "Erreur supDernier, il n'y à pas d'element"
  | e::[] -> []
  | e::s -> e::(supDernier s)

(* Supprime la dernière case du serpent *)
let supSerp s = {s with composition = (supDernier s.composition)}

(* Appel la fonction f i fois sur elem*)
let rec repFonction f i elem =
  if i = 0 then elem
  else repFonction f (i-1) (f elem)

(*
  Déplace le serpent en fonction de sont orientation. 
  S'il ne rencontre pas de pastille, on supprime le dernier element pour évité qu'il grandisse en continu.
  
  Vérifie si le déplacement entraine le serpent dans un bord, dans un obstacle, sur lui même ou dans un autre joueur
*)
let serpSuiv s damier = 
  let caseSuiv = recupProchaineCase s damier in
    match caseSuiv.contenant with
        | Vide -> avancer (supSerp s) caseSuiv
        | Pastille ->  (match avancer s caseSuiv with serp -> {serp with score = s.score+1})
        | Bord -> failwith ("Perdu ! Le "^s.appartient.nom^" à touché le bord du terrain.")
        | Serpent j-> if j = s.appartient then failwith ("Perdu ! "^s.appartient.nom^" s'est manger lui même.")
                        else failwith ("Perdu ! "^s.appartient.nom^" est entré en colision avec "^j.nom)
        | Obstacle -> failwith ("Perdu ! Le "^s.appartient.nom^" à touché un obstacle.")

(* 
  Applique serpSuiv avec une liste de serpent 
  Retourne la liste des serpent avec leurs nouveaux déplacement
*)
let serpSuivList ls damier = List.fold_left (fun acc s -> acc @ [(serpSuiv s damier)]) [] ls



(* Fait la somme des score de la liste de serpent. Utilisé pour savoir quand augmenter la vitesse du jeux *)
let sumScore lserp = List.fold_left (fun acc serp -> acc + serp.score) 0 lserp


(*------------------------------ fonction dessin ------------------------------*)
(* Dessine un carrer plein représentant une case.
  La couleur varie en fonction du contenu d'une case*)
let dessinerCase case = match case.coord with
  (x,y) -> match case.contenant with
    Vide -> set_color white ; fill_rect (x*tCase) (y*tCase) tCase tCase
    | Bord -> set_color black;fill_rect (x*tCase) (y*tCase) tCase tCase
    | Pastille -> set_color green;fill_rect (x*tCase) (y*tCase) tCase tCase
    | Obstacle -> set_color red;fill_rect (x*tCase) (y*tCase) tCase tCase
    | Serpent j-> set_color j.couleurSerp;
                  fill_rect (x*tCase) (y*tCase) tCase tCase;
                  set_color green;
                  draw_rect (x*tCase) (y*tCase) tCase tCase
                  
                  
                  
                  
(* Dessiner les cases ce trouvant dans liste *)
let dessinerListeCase liste = List.fold_left (fun acc case  -> dessinerCase case) () liste

(* Afficher dans le coin en (x,y) de la fenetre un msg*)
let afficherMsg (x,y) msg =
  let tx,ty = text_size msg in
      set_color white;
      fill_rect x y tx ty;
      set_color black;
      moveto x y ;
      draw_string msg

(* Affichage du score en haut à droite de la fenètre *)
let afficherScore s pos = afficherMsg pos ("Score "^s.appartient.nom^" : " ^ (string_of_int s.score))

(* Affiche la liste des score en haut à droite*)
let afficherPlusieurScore lSerp = 
  let nbrScore = List.length lSerp in
    for i = 0 to nbrScore-1 do
      afficherScore (List.nth lSerp i) (width-110,(height-(i+1)*20+5))
    done
  

(* Génération d'un état initial du jeu *)
let init_state () = 
  let damierVide = genCases (lDamier) hDamier in 
    let damierAvecBord = genererBord damierVide in
      let damierAvecSerp = insererPlusieurSerp listeSerpIni damierAvecBord in 
        let damierAvecPastille =  repFonction (genererContenantCase Pastille) nbrPastille damierAvecSerp in
          let damierAvecObstacle = ajouterObstacle obstacle damierAvecPastille in
            {damier = damierAvecObstacle;
              listeSerp = listeSerpIni}


(* Affichage du plateau initial à partir de l'état initial. *)
let init_drawing state =
  dessinerListeCase state.damier;
  afficherPlusieurScore state.listeSerp



(* Cette fonction est appelée à chaque pas de jeu
  avec l'état courant et elle doit retourner le
  nouvel état du jeu.
  Elle doit également mettre à jour l'affichage
  en conséquence.
  Si le nouvel état correspond à une fin de jeu,
  cette fonction peut retourner une exception en
  appelant [failwith] suivi d'un message (ex.,
  "Perdu! le serpent a touché le bord.". *)
let make_step state =
  let etatSuivant =
    (
      let listeSerpSuiv = serpSuivList state.listeSerp state.damier in
          let damierAvecSerp = insererPlusieurSerp listeSerpSuiv state.damier in
              if (nbrCase Pastille damierAvecSerp) = 0 then
                let damierAvecPastille = repFonction (genererContenantCase Pastille) nbrPastille damierAvecSerp in
                  let _ = (* let pour enlever le warning lors de la compilation *)
                  (* augmente a vitesse du serpent toutes les 'nbrPastille' pastilles récupérer *)
                  Unix.setitimer Unix.ITIMER_REAL 
                    {Unix.it_interval = itInterval /. (1.3 *. float_of_int (sumScore listeSerpSuiv/nbrPastille));
                      Unix.it_value = 0.00000001};
                  in
                  {damier = damierAvecPastille;
                      listeSerp = listeSerpSuiv}

              else {damier = damierAvecSerp; 
                    listeSerp = listeSerpSuiv}
    )
  in dessinerListeCase etatSuivant.damier;
    afficherPlusieurScore etatSuivant.listeSerp;
    etatSuivant

(* ------------------------------ fonction direction ------------------------------ *)
(* Tourne à gauche la tête du serpent *)
let tGauche s = match s.dir with
    Nord -> {s with dir = Ouest}
  | Sud -> {s with dir = Est}
  | Est -> {s with dir = Nord}
  | Ouest -> {s with dir = Sud}


(* Tourne à droite la tête du serpent *)
let tDroite s = match s.dir with
    Nord -> {s with dir = Est}
  | Sud -> {s with dir = Ouest}
  | Est -> {s with dir = Sud}
  | Ouest -> {s with dir = Nord}


(* Action lorsqu'un joueur veut aller vers le haut *)
let versHaut s = match s.dir with
  | Nord -> s (* rien à faire, le serpent va déja vers le haut *)
  | Sud -> s (* impossible de faire un demi-tour complet*)
  | Est -> tGauche s
  | Ouest -> tDroite s

(* Action lorsqu'un joueur veut aller vers la gauche *)
let versGauche s = match s.dir with
                | Nord -> tGauche s
                | Sud -> tDroite s
                | Est -> s
                | Ouest -> s


(* Action lorsqu'un joueur veut aller vers le bas *)
let versBas s = match s.dir with
                | Nord -> s
                | Sud -> s
                | Est -> tDroite s
                | Ouest -> tGauche s

(* Action lorsqu'un joueur veut aller vers la droite *)
let versDroite s = match s.dir with
                | Nord -> tDroite s
                | Sud -> tGauche s
                | Est -> s
                | Ouest -> s

(*  *)
let appliqueTouche car s = 
  if car = s.appartient.haut then versHaut s
  else if car = s.appartient.bas then versBas s
  else if car = s.appartient.gauche then versGauche s
  else if car = s.appartient.droite then versDroite s
  else s

let appliquerToucheSerps car lSerp = List.map (appliqueTouche car) lSerp



(* modifie la liste l en appliquant la fonction f à l'element d'indice i.
  La fonction renvoi la liste si i est trop grand et non une erreur pour éviter 
  d'arreter le programme si par exemple il n'y à qu'un jouer et que l'on appuis 
  sur une touche attribué pour le joueur 2*)
let rec modifElemList f l i = match l with
| [] -> l
| e::s  -> if i = 0 then (f e)::s
            else e::(modifElemList f s (i-1))

(* boucle d'exécution et d'intéraction *)
let rec loop ref_state = 
  let status = wait_next_event [Key_pressed] in (* attendre la frappe d'une touche *)
  let state = !ref_state in
  let state =
    match status.key with
    | 't' -> failwith "Bye bye!" (* la touche 't' permet de quitter le programme *)
      (* autres touches *)
          (* produire un nouvel état en fonction du changement de direction *)
    (* choisir des touches/caractères pour chaque direction *)

    (* déplacement serpent *)
    | touche -> {state with listeSerp = (appliquerToucheSerps touche state.listeSerp)}
    in
  ref_state := state;
  loop ref_state

(* fonction principale *)
let main () =
  Random.self_init (); (* initialisation du générateur aléatoire *)
  let ref_state = ref (init_state ()) in (* génération état initial *)
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  auto_synchronize false; (* faire les modification de l'affichage en arriere-plan *)
  init_drawing !ref_state; (* affichage initial *)
  let _ = Unix.setitimer Unix.ITIMER_REAL (* installation d'un timer tous les 1/10emes de secondes... *)
    { Unix.it_interval = itInterval;
      Unix.it_value = 0.1 } in
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ ->
      try
  ref_state := make_step !ref_state; (* ... pour mettre à jour l'état et l'affichage *)
  synchronize () (* rendre les modifications de l'affichage visible *)
      with
      | Failure msg -> (* en cas de fin de partie *)
  print_endline msg; (* afficher un message d'erreur *)
  exit 0)); (* quitter le jeu *)
  loop ref_state (* boucle d'interaction *)

(* appel de la fonction principale *)


let _ = main ()
