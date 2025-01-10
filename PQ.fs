module PQ

type 'a PQ = Map<System.IComparable, 'a list>

let length (pq: 'a PQ) : int =
    pq |> (Map.values >> List.concat >> List.length)

let tryPop (q: 'a PQ) : ('a PQ * 'a) option =
    if Map.isEmpty q then
        None
    else
        match Map.minKeyValue q with
        | _, [] -> None
        | k, [ head ] -> Some(Map.remove k q, head)
        | k, head :: rest -> Some(Map.add k rest q, head)

let push (priority: System.IComparable) (value: 'a) : 'a PQ -> 'a PQ =
    let change =
        function
        | Some values -> value :: values
        | None -> [ value ]
        >> Some

    Map.change priority change
