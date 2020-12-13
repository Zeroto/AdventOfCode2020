module Day13
open System.IO
open System

type Bus =
  | Id of int
  | X

let parseInput (s: string array) =
  let timestamp = int s.[0]
  let busses =
    s.[1].Split(',')
    |> Array.map (fun b ->
      match b with
      | "x" -> X
      | v -> int v |> Id
    )

  (timestamp, busses)

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let timestamp, busses = input
    let nextBusTimes =
      busses
      |> Array.choose (fun bus ->
        match bus with
        | Id i ->
          let t = timestamp / i
          Some (i, i * (t+1) - timestamp)
        | X -> None
      )
    let result =
      nextBusTimes
      |> Array.minBy snd
      |> fun (id, minutes) -> id * minutes
    sprintf "Result: %d" result
  | "b" ->
    "Solved by hand using chinese remainder theorem"
  | _ -> "Invalid Part input"