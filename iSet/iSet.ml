(* Łukasz Raszkiewicz
   Review: Albert Cieslak *)

(* interval(a,b) oznacza przedzial [a;b] *)
type interval = int * int

(* porownywanie przedzialow: -1 gdy a wczesniejszy niz b,
   0 gdy sie niepusto przecinaja, 1 gdy b wczesniejszy niz a *)
let cmp (a1,a2) (b1,b2) =
  if a2 < b1 then -1
  else if b2 < a1 then 1
  else 0

(* jako elementy nazywam liczby w przedzialach, tj. drzewo
   skladajace sie tylko z wierzcholka [1;5] zawiera 5 elementow *)

(* typ do trzymania drzewa: albo jest puste, albo jest wierzcholkiem postaci:

   (lewe poddrzewo, przedzial w wierzcholku, prawe poddrzewo,
   (wysokosc drzewa, liczba elementow w drzewie))

   dwa ostatnie inty trzymam jako pare, zeby nie modyfikowac kodu,
   w ktorym autor uzyl matchowania Node(l,k,r,_) *)
type t =
    | Empty
    | Node of t * interval * t * (int * int)

(* stworz puste drzewo *)
let empty = Empty

(* sprawdz, czy drzewo jest puste *)
let is_empty x = (x = Empty)

(* wysokosc drzewa *)
let height = function
  | Node (_, _, _, (h, _)) -> h
  | Empty -> 0

(* liczba elementow w przedziale;
   jezeli jest dluzszy niz max_int, zwroc max_int *)
let interval_size (a, b) =
  if (-a >= max_int-b-1) || (a == min_int && b >= -1) 
  then max_int else b-a+1

(* dodaj dwie liczby calkowite nie przekraczajac max_int *)
let add_integers a b =
  if a >= max_int-b then max_int else a+b

(* liczba elementow w drzewie *)
let set_size = function
  | Node (_, _, _, (_, s)) -> s
  | Empty -> 0

(* stworz drzewo z lewego poddrzewa, wartosci korzenia i prawego poddrzewa *)
let make l k r =
  Node (l, k, r,
        ((max (height l) (height r) + 1),
         (add_integers
            (add_integers (set_size l) (set_size r))
            (interval_size k))))

(* zbalansuj drzewo *)
let bal l k r =
  let hl = height l in
  let hr = height r in
    if hl > hr + 2 then
      match l with
        | Node (ll, lk, lr, _) ->
            if height ll >= height lr then make ll lk (make lr k r)
            else
              (match lr with
                | Node (lrl, lrk, lrr, _) ->
                    make (make ll lk lrl) lrk (make lrr k r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
      match r with
        | Node (rl, rk, rr, _) ->
            if height rr >= height rl then make (make l k rl) rk rr
            else
              (match rl with
                | Node (rll, rlk, rlr, _) ->
                    make (make l k rll) rlk (make rlr rk rr)
                | Empty -> assert false)
        | Empty -> assert false
    else make l k r

(* najwczesniejszy przedzial w drzewie *)
let rec min_elt = function
  | Node (Empty, k, _, _) -> k
  | Node (l, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* usun najwczesniejszy przedzial w drzewie *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _) -> r
  | Node (l, k, r, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

(* polacz drzewa t1, t2, ktore nie maja wspolnych elementow *)
let merge t1 t2 =
  match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | _ ->
        let k = min_elt t2 in
          bal t1 k (remove_min_elt t2)

(* dodaj do drzewa tree jeden przedzial x,
   ktory nie ma z tym drzewem wspolnych elementow
   oraz nie bedzie sasiadujacych przedzialow *)
let rec add_one x tree =
  match tree with
    | Node (l, k, r, h) ->
        let c = cmp x k in
          assert (c <> 0);
          if c < 0 then
            let nl = add_one x l in
              bal nl k r
          else (* c = 0 nie zachodzi *)
            let nr = add_one x r in
              bal l k nr
    | Empty -> make empty x empty

(* laczenie dwoch drzew i przedzialu, ktore nie zawieraja
   takich samych elementow
   ani sasiadujacych przedzialow *)
let rec join l v r =
  match (l, r) with
    | (Empty, _) -> add_one v r
    | (_, Empty) -> add_one v l
    | (Node(ll, lv, lr, (lh, _)), Node(rl, rv, rr, (rh, _))) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
          make l v r

(* sprawdzam, czy przedzial zawiera >0 elementow *)
let valid_interval i =
  match i with
    | (Some(i1),Some(i2)) ->
        if interval_size (i1,i2) > 0 then true else false
    | _ -> false

(* konwertuje z option int * option int do int * int
   z zalozeniem, ze oba elementy to Some(int) *)
let convert_to_interval i =
  match i with
    | Some(i1),Some(i2) -> (i1,i2)
    | _ -> assert false

(* podziel drzewo na dwa drzewa: jedno z elementami <x, drugie >x
   oraz bool czy x byl zawarty w drzewie *)
let split x s =
  let rec loop s =
    match s with
      | Empty -> (Empty, false, Empty)
      | Node (l, v, r, _) ->
          let c = cmp (x,x) v in
            if c = 0 then
              let (v1,v2) = v in
              let p1 = (Some(v1),(if x <> min_int then Some(x-1) else None))
              and p2 = ((if x <> max_int then Some(x+1) else None),Some(v2)) in
                ((if valid_interval p1 then
                    add_one (convert_to_interval p1) l else l),
                 true,
                 (if valid_interval p2 then
                    add_one (convert_to_interval p2) r else r))
            else if c < 0 then
              let (ll, pres, rl) = loop l in (ll, pres, join rl v r)
            else
              let (lr, pres, rr) = loop r in (join l v lr, pres, rr)
  in loop s

(* usun przedzial z drzewa *)
let remove (x1,x2) s =
  let (l, _, _) = split x1 s
  and (_, _, r) = split x2 s
  in merge l r

(* sprawdzam, czy element x jest w drzewie;
   jesli tak, to w jakim przedziale *)
let mem_with_interval x s = 
  let x = (x,x) in
  let rec loop s =
    match s with
      | Node (l, k, r, _) ->
          let c = cmp x k in
            if c = 0 then Some k
            else loop (if c < 0 then l else r)
      | Empty -> None
  in loop s

(* sprawdzam, czy element x jest w drzewie *)
let mem x s = mem_with_interval x s != None

(* iter, fold, elements niezmienione z poprzedniej implementacji
   poza poprawkami czytelnosci i dopasowania typow *)
let iter f s =
  let rec loop s =
    match s with
      | Empty -> ()
      | Node (l, k, r, _) -> loop l; f k; loop r
  in loop s

let fold f s acc =
  let rec loop acc s =
    match s with
      | Empty -> acc
      | Node (l, k, r, _) -> loop (f k (loop acc l)) r
  in loop acc s

let elements s = 
  let rec loop acc s =
    match s with
      | Empty -> acc
      | Node(l, k, r, _) -> loop (k :: loop acc r) l
  in loop [] s

(* liczba elementow nie wiekszych niz x *)
let below x s =
  let (l,b,_) = split x s in
    add_integers (set_size l) (if b then 1 else 0)

(* dodaj do drzewa przedzial, ktory moze miec z drzewem
   niepuste przeciecie *)
let add (x1,x2) s =
  let left =
    match mem_with_interval (x1-1) s with
      | None -> x1
      | Some(a1,a2) -> a1
  and right =
    match mem_with_interval (x2+1) s with
      | None -> x2
      | Some(b1,b2) -> b2
  in add_one (left,right) (remove (left,right) s)
