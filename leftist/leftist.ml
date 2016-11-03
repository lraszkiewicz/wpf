(* Łukasz Raszkiewicz
   Code review: Aleksander Zendel *)

type 'a queue =
    | Null
    | Node of 'a * ('a queue) * ('a queue) * int;;

let empty = Null;;

let is_empty q =
  match q with
    | Null -> true
    | _ -> false;;

let rec join q1 q2 =
  match (q1,q2) with
    | (Null,q) -> q
    | (q,Null) -> q
    | (Node(e,left1,right1,_), Node(e2,_,_,_)) ->
        if e2 < e then
          join q2 q1
        else
          let q3 = join right1 q2 in
            match (left1,q3) with
              | (Null,q) -> Node(e,q,Null,0)
              | (q,Null) -> Node(e,q,Null,0)
              | (Node(_,_,_,rank1), Node(_,_,_,rank2)) ->
                  if rank1 > rank2 then
                    Node(e,left1,q3,rank2+1)
                  else
                    Node(e,q3,left1,rank1+1);;

let add e q =
  join (Node(e,Null,Null,0)) q;;

exception Empty;;

let delete_min q =
  match q with
    | Null -> raise Empty
    | Node(e,left,right,_) -> (e, join left right);;
