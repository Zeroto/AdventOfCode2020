module Day15
open System.IO
open System

let parseInput = Array.map int64 >> Array.mapi (fun i k -> (k,int64 i))

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let seed = input |> Array.rev |> Array.tail |> Array.rev |> Map.ofArray
    let rec iterate i lastNumber numbers =
      if i = 2019L then
        lastNumber
      else
        let lastTimeSpoken = numbers |> Map.tryFind lastNumber |> Option.defaultValue i
        let difference = i - lastTimeSpoken
        let newMap = numbers |> Map.add lastNumber i
        iterate (i+1L) difference newMap

    let result = iterate (int64 (input |> Array.length)-1L) (input |> Array.last |> fst) seed
    sprintf "Result: %d" result
  | "b" ->
    let seed = input |> Array.rev |> Array.tail |> Array.rev |> Map.ofArray
    let rec iterate i lastNumber numbers =
      if i % 100000L = 0L then printfn "%d" i
      if i = 29999999L then
        lastNumber
      else
        let lastTimeSpoken = numbers |> Map.tryFind lastNumber |> Option.defaultValue i
        let difference = i - lastTimeSpoken
        let newMap = numbers |> Map.add lastNumber i
        iterate (i+1L) difference newMap

    let result = iterate (int64 (input |> Array.length)-1L) (input |> Array.last |> fst) seed
    sprintf "Result: %d" result
  | _ -> "Invalid Part input"