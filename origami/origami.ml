(* Lukasz Raszkiewicz
   Review: Michal Borkowski *)

type point = float * float;;

type kartka = point -> int;;

let eps = 1e-7;;

let (=.) a b =
  let t = a-.b in
    if t >= -.eps && t <= eps then true else false;;

let (<=.) a b =
  (a<b) || (a=.b);;

let prostokat (ax,ay) (bx,by) =
  fun (px,py) ->
    if (ax<=.px && px<=.bx && ay<=.py && py<=.by) then 1 else 0;;

let kw x = x*.x;;

let kolko (ax,ay) r =
  fun (px,py) ->
    if (kw (px-.ax))+.(kw (py-.ay)) <=. kw r then 1 else 0;;

(* poczatek geometrii *)
let strona (ax,ay) (bx,by) (cx,cy) =
  let x = ((bx-.ax)*.(cy-.ay) -. (by-.ay)*.(cx-.ax)) in
    if x =. 0. then 0
    else if x < 0. then -1
    else 1;;
(* 1 to lewa, -1 to prawa, 0 to na prostej *)

let skalarny (ax,ay) (bx,by) (cx,cy) =
  (bx-.ax)*.(cx-.ax) +. (by-.ay)*.(cy-.ay);;

let rzut (ax,ay) (bx,by) (cx,cy) =
  let t = (skalarny (ax,ay) (bx,by) (cx,cy))/.(skalarny (ax,ay) (bx,by) (bx,by)) in
    (* dzielenie przez 0 tylko dla a = b, co nie moze zajsc *)
    (ax+.t*.(bx-.ax), ay+.t*.(by-.ay));;

let symetria_punkt (sx,sy) (ax,ay) =
  (2.*.sx-.ax, 2.*.sy-.ay);;

let symetria_prosta a b p =
  symetria_punkt (rzut a b p) p;;
(* koniec geometrii *)

let zloz a b k =
  fun p ->
    match (strona a b p) with
      | 0 -> k p
      | 1 -> k p + k (symetria_prosta a b p)
      | _ -> 0;; (* to tak naprawde jest | -1 -> 0, ale nie chce warninga *)

let skladaj l k =
  List.fold_left (fun a (p1,p2) -> zloz p1 p2 a) k l;;

(* TESTY *)

(* punkt jest przecieciem wszystkich prostych z listy *)
1 = (skladaj
       [((8.,4.),(6.,2.)); ((12.,11.),(2.,-5.)); ((5.,7.), (9.,-1.));
        ((4.,12.),(10.,-6.)); ((11.,-4.),(3.,10.))]
       (prostokat (2.,1.) (8.,5.)))
      (7.,3.);;

(* jak wyzej, ale z kolkiem *)
1 = (skladaj
       [((8.,4.),(6.,2.)); ((12.,11.),(2.,-5.)); ((5.,7.), (9.,-1.));
        ((4.,12.),(10.,-6.)); ((11.,-4.),(3.,10.))]
       (kolko (9.,2.) 2.5))
      (7.,3.);;

(* kwadrat zgiety przez przekatna, punkt na krawedzi *)
2 = (skladaj
       [((2.0,8.0),(5.0,5.0))]
       (prostokat (0.,0.) (10.,10.)))
      (10.0,7.0);;

(* rozne "losowe" testy (czyli narysowane i wpisane z reki) *)
8 = (skladaj
       [((4.,-1.),(12.,-1.)); ((6.,-3.),(6.,2.));
        ((5.,-3.),(7.,4.)); ((7.,-3.),(4.,4.))]
       (prostokat (2.,-2.) (8.,3.)))
      (5.6,-0.2);;

0 = (skladaj
       [((4.,-1.),(12.,-1.)); ((6.,-3.),(6.,2.));
        ((5.,-3.),(7.,4.)); ((7.,-3.),(4.,4.))]
       (prostokat (2.,-2.) (8.,3.)))
      (7.,0.);;

4 = (skladaj
       [((6.,-1.),(6.,3.)); ((0.,2.),(1.,2.))]
       (prostokat (2.,0.) (8.,4.)))
      (5.,3.);;

2 = (skladaj
       [((6.,-1.),(6.,3.)); ((0.,2.),(1.,2.))]
       (prostokat (2.,0.) (8.,4.)))
      (3.,3.);;

4 = (skladaj
       [((3.,0.),(6.,2.)); ((7.,-1.),(3.,2.))]
       (kolko (4.,2.) 3.))
      (4.,1.);;
