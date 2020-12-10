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

let sqrt33 = Math.Sqrt 33.0
let denom = (Math.Pow(586.0 + 102.0*sqrt33, 2.0/3.0) + 4.0 - 2.0 * Math.Pow(586.0 + 102.0 * sqrt33, 1.0/3.0))
let alpha = 1.0/3.0*Math.Pow(19.0+3.0*sqrt33, 1.0/3.0) + 1.0/3.0*Math.Pow(19.0-3.0*sqrt33, 1.0/3.0) + 1.0/3.0
let beta = Math.Pow(586.0 + 102.0 * sqrt33, 1.0/3.0)
let tribonacci n =
  Math.Round ((Math.Pow(alpha, float n) * beta) / denom * 3.0)
  |> int64

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  [1..10]
  |> List.map tribonacci
  |> printfn "%A"

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
    let countToPermCount i = tribonacci i
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