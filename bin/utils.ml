let get_priority line =
    if
      String.length line >= 4
      && line.[0] = '('
      && line.[2] = ')'
      && line.[3] = ' '
      && line.[1] >= 'A'
      && line.[1] <= 'Z'
    then Some line.[1]
    else None

let compare_by_priority a b =
    match (get_priority a, get_priority b) with
    | Some prio_a, Some prio_b -> compare prio_a prio_b
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0
