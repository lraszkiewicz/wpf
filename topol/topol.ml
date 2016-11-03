(*	Łukasz Raszkiewicz
	Review: Miłosz Rzeźnikowski *)

open PMap;;

exception Cykliczne;;

let topol l =
	let graph = ref empty in
	let visited = ref empty in
	let marked = ref empty in
	let result = ref [] in
	let rec visit a =
		if (find a (!marked)) = true then
			raise Cykliczne
		else if (find a (!visited)) = false then (
			marked := add a true (!marked);
			List.iter (fun b -> visit b) (find a (!graph));
			marked := add a false (!marked);
			visited := add a true (!visited);
			result := a::(!result)) in
	List.iter (fun (_,al) ->
		List.iter (fun b -> graph := add b [] (!graph)) al) l;
	List.iter (fun (a,al) -> graph := add a al (!graph)) l;
	iter (fun a _ -> visited := add a false (!visited)) (!graph);
	iter (fun a _ -> marked := add a false (!marked)) (!graph);
	iter (fun a _ -> if (find a (!visited)) = false then visit a) (!graph);
	(!result);;
