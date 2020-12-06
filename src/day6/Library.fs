module Day6

let parseInput (s: string array) =
  (([] , []),s)
  ||> Array.fold 
    ( fun (s, g) l ->
      if l = "" then
        (s @ [g], [])
      else
        (s, g @ [l |> Seq.distinct])
    )
  |> fst

let main (argv: string array) =
  let input =
    System.IO.File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let result =
      input
      |> List.sumBy (Seq.concat >> Seq.distinct >> Seq.length)
    sprintf "Result: %d" result
  | "b" ->
    let result =
      input
      |> List.sumBy (fun g ->
        let groupCount = g |> List.length
        g |> Seq.concat |> Seq.countBy id |> Seq.filter (fun (_, v) -> v = groupCount) |> Seq.length)
    sprintf "Result: %d" result
  | _ -> "invalid part input"
    