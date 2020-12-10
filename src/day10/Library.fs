module Day10
open System.IO
open System

let parseInput = Array.map int

let goldenRatio = 1.618034
let sqrt5 = Math.Sqrt(5.0)
let Fibonacci n =
  (Math.Pow(goldenRatio, float n) - Math.Pow((1.0 - goldenRatio), float n)) / sqrt5
  |> Math.Round
  |> int64

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
    let countToPermCount i = Math.Max((Fibonacci (i+1)) - 1L, 1L)
    let max = input |> Array.max
    let input = input |> Array.append [|0; max + 3|] |> Array.sort |> Array.pairwise
    let result =
      ((1, 1L), input)
      ||> Array.fold (fun (count, p) (a,b) -> 
        if b-a = 1 then
          (count + 1, p)
        else
          (1, p*(countToPermCount count))
      )
    sprintf "Result: %d" (snd result)
  | _ -> "Invalid Part input"