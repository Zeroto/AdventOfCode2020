module Day5

let parseInput (s: string array) =
  s
  |> Array.map
    (fun l ->
      ((0, 0), l |> Seq.rev)
      ||> Seq.fold
        (fun (s,i) c ->
          let v =
            match c with
            | 'B' | 'R' -> 1
            | 'F' | 'L' -> 0
            | _ -> failwithf "Invalid input: %c" c
          (s + (v<<<i), i+1)
        )
      |> fst
    )

let main (argv: string array) =
  let input =
    System.IO.File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let result = input |> Array.max
    sprintf "Result: %d" result
  | "b" ->
    let sortedInput =
      input
      |> Array.sort
      |> Array.pairwise

    let (leftSeat, _) =
      sortedInput
      |> Array.find (fun (a,b) -> b-a = 2)
    
    sprintf "Result: %d" (leftSeat+1)

  | _ -> "Invalid part input"