module Day1

open System

let parseInt s =
  match Int32.TryParse s with
  | true, v -> Some v
  | false, _ -> None

let main (argv: string array) =
  let input =
    System.IO.File.ReadAllText argv.[1]
    |> (fun s -> s.Split('\n'))
    |> Array.choose parseInt
    |> Array.indexed

  match argv.[0] with
  | "a" ->
    input
    |> Array.pick
        (fun (i, v) ->
          input.[i+1..]
          |> Array.tryFind (fun (_, s) -> v+s = 2020)
          |> Option.map (fun (_, s) -> v*s)
        )
    |> printfn "result: %d"
    0
  | "b" ->
    input
    |> Array.pick
        (fun (i, a) ->
          input.[i+1..]
          |> Array.tryPick
            (fun (i, b) ->
              input.[i+1..]
              |> Array.tryFind (fun (_, c) -> a+b+c = 2020)
              |> Option.map (fun (_, c) -> a*b*c)
            )
        )
    |> printfn "result: %d"
    0
  | _ -> 0