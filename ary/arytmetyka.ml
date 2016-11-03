(* Łukasz Raszkiewicz
   Code review: Tomasz Garbus *)

type wartosc =
    | Spojny of float*float (* x zawiera sie w [a;b]
                               w szczegolnosci moze wystapic sytuacja, ze a,b E {inf,-inf} *)
    | Niespojny of float*float (* x zawiera sie w [-inf;a] U [b;inf] *)
    | Nan;;

let wartosc_dokladnosc x p =
  Spojny(x-.(abs_float(x*.(p/.100.))), x+.abs_float((x*.(p/.100.))));;

let wartosc_od_do x y =
  Spojny(x, y);;

let wartosc_dokladna x =
  Spojny(x, x);;

let in_wartosc w x =
  match w with
    | Spojny(a,b) ->
        if ((x >= a) && (x <= b)) then 
          true
        else
          false
    | Niespojny(a,b) ->
        if ((x <= a) || (x >= b)) then
          true
        else
          false
    | Nan -> false;;

let min_wartosc w =
  match w with
    | Spojny(a,b) -> if (a = 0.) then 0. else a
    | Niespojny(_,_) -> neg_infinity
    | Nan -> nan;;

let max_wartosc w =
  match w with
    | Spojny(a,b) -> if (b = 0.) then 0. else b
    | Niespojny(_,_) -> infinity
    | Nan -> nan;;

let sr_wartosc w =
  match w with
    | Spojny(a,b) -> let c = ((a+.b)/.2.) in (if (c = 0.) then 0. else c)
    | _ -> nan;;

let uprosc w =
  match w with
    | Niespojny(a,b) ->
        if a >= b then
          Spojny(neg_infinity,infinity)
        else
          w
    | _ -> w;;

let rec plus w v =
  match (w,v) with
    | (Spojny(a,b), Spojny(c,d)) -> Spojny(a+.c, b+.d)
    | (Niespojny(a,b), Spojny(c,d)) -> uprosc(Niespojny(a+.d, b+.c))
    | (Spojny(_), Niespojny(_)) -> plus v w
    | (Niespojny(a,b), Niespojny(c,d)) -> Spojny(neg_infinity,infinity)
    | _ -> Nan;;

let minus w v =
  plus w
    (match v with
      | Spojny(a,b) -> Spojny(-.b,-.a)
      | Niespojny(a,b) -> Niespojny(-.b,-.a)
      | _ -> Nan);;

let min_fixed a b =
  if a <> a then b
  else if b <> b then a
  else min a b;;

let max_fixed a b =
  if a <> a then b
  else if b <> b then a
  else max a b;;

let min_list l =
  List.fold_left (min_fixed) infinity l;;

let max_list l =
  List.fold_left (max_fixed) neg_infinity l;;

let przeciwny w =
  match w with
    | Spojny(a,b) -> Spojny(-.b,-.a)
    | Niespojny(a,b) -> Niespojny(-.b,-.a)
    | _ -> w;;

let rec razy w v =
  if ((w = Nan) || (v = Nan)) then Nan
  else if((w = Spojny(0.,0.)) || (v = Spojny(0.,0.))) then
    Spojny(0.,0.)
  else
    match (w,v) with
      | (Spojny(a,b), Spojny(c,d)) -> Spojny(min_list [a*.c;a*.d;b*.c;b*.d], max_list [a*.c;a*.d;b*.c;b*.d])
      | (Niespojny(a,b), Spojny(c,d)) -> 
          if (c <= 0. && d <= 0.) then
            razy (przeciwny w) (przeciwny v)
          else 
            uprosc(Niespojny(max_fixed (a*.c) (a*.d), min_fixed (b*.c) (b*.d)))
      | (Spojny(_), Niespojny(_)) -> razy v w
      | (Niespojny(a,b), Niespojny(c,d)) -> uprosc(Niespojny(max_fixed (a*.d) (c*.b), min_fixed (a*.c) (b*.d)))
      | _ -> Nan;;

let podzielic w v =
  razy w
    (match v with
      | Spojny(0.,0.) -> Nan
      | Spojny(a,0.) -> Spojny(neg_infinity,1./.a)
      | Spojny(0.,b) -> Spojny(1./.b,infinity)
      | Spojny(a,b) -> (if (a < 0. && b > 0.) then uprosc(Niespojny(1./.a,1./.b)) else Spojny(1./.b,1./.a))
      | Niespojny(a,b) -> (if((a > 0.) || (b < 0.)) then uprosc(Niespojny(1./.b,1./.a)) else Spojny(1./.a,1./.b))
      | _ -> Nan);;
