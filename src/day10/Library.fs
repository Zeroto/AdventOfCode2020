module Day10
open System.IO

let parseInput = Array.map int

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let counts =
      input
      |> Array.append [|0|]
      |> Array.sort
      |> Array.pairwise
      |> Array.countBy (fun (a,b) -> b-a)
      |> Map.ofArray

    let result = (counts |> Map.find 1) * ((counts |> Map.find 3) + 1)
    sprintf "Result: %d" result
  | "b" ->
    let countToPermCount i =
      match i with
      | 0 | 1 | 2 -> 1L
      | 3 -> 2L
      | 4 -> 4L
      | 5 -> 7L
      | 6 -> 12L
      | _ -> failwithf "Bigger difference: %d" i

    let max = input |> Array.max
    let input = input |> Array.append [|0; max + 3|] |> Array.sort |> Array.pairwise
    let rec solver index product =
      if index >= Array.length input then
        product
      else
        let count =
          input.[index..]
          |> Array.findIndex (fun (a,b) -> b-a <> 1)
          |> (+) 1
        printfn "count: %d" (countToPermCount count)
        solver (index+count) (product * (countToPermCount count))
    let result = solver 0 1L
    sprintf "Result: %d" result
      
  | _ -> "Invalid Part input"