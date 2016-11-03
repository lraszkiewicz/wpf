(*	Łukasz Raszkiewicz
	Review: Rafał Brzeziński *)

let przelewanka t =
	let visited = Hashtbl.create 10000 in
	let q = Queue.create () in
	let n = Array.length t in
	let target = Array.make n 0 in
	(*	stan z pustymi szklankami dodaje na początek kolejki,
		"odległość" do niego to 0 *)
	Queue.add (Array.copy target, 0) q;
	(* a teraz w tablicy target umieszczam docelowy stan, przyda się później *)
	for i = 0 to (n-1) do
		target.(i) <- snd t.(i)
	done;
	(*	visit będzie służyło do dodawania "wierzchołka" (tzn. stanu) na kolejkę,
		jeżeli nie był odwiedzony oraz do oznaczania go jako odwiedzonego *)
	let visit temp d =
		if not (try Hashtbl.find visited temp with
					Not_found -> false)
		then (
			Queue.add (temp, d+1) q;
			Hashtbl.add visited temp true;
		) in
	let result = ref (-1) in
	(* zaczynam BFSa *)
	while not (Queue.is_empty q) do (
		let (s, d) = Queue.take q in
		if s = target then (
			result := d;
			Queue.clear q;
		)
		else (
			(* czynność 1: wlanie wody do szklanki z kranu *)
			for i = 0 to (n-1) do
				if s.(i) < fst t.(i) then ( (* nie napełniam pełnych szklanek *)
					let temp = Array.copy s in
					temp.(i) <- fst t.(i);
					visit temp d;
				)
			done;
			(* czynność 2: wylanie wody ze szklanki do zlewu *)
			for i = 0 to (n-1) do
				if s.(i) > 0 then ( (* nie wylewam z pustych szklanek *)
					let temp = Array.copy s in
					temp.(i) <- 0;
					visit temp d;
				)
			done;
			(* czynność 3: przelanie ze szklanki i do szklanki j *)
			for i = 0 to (n-1) do
				for j = 0 to (n-1) do
					(*	nie przelewam ze szklanki do samej siebie,
						nie przelewam z pustych szklanek,
						nie przelewam do pełnych szklanek *)
					if i <> j && s.(i) > 0 && s.(j) < fst t.(j) then (
						let temp = Array.copy s in
						(*	przelewam tyle ile się zmieści w szklance j,
							chyba że tyle nie mam *)
						let przelewam = (min ((fst t.(j))-s.(j)) (s.(i))) in
						temp.(i) <- s.(i) - przelewam;
						temp.(j) <- s.(j) + przelewam;
						visit temp d;
					)
				done;
			done;
		)
	) done;
	!result;;
