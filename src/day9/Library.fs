module Day9
open System.IO

let parseInput = Array.map int64

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let check =
      input
      |> Array.windowed 26
      |> Array.pick
        (fun data ->
          let checksum = data |> Array.last
          let notFound =
            data.[0..24]
            |> Array.allPairs data.[0..24]
            |> Array.exists (fun (a,b) -> a+b = checksum)
            |> not
          if notFound then
            Some checksum
          else
            None
        )
    sprintf "Result: %d" check
  | "b" ->
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let target = 90433990L
    let rec findRangeSum start ``end`` sum =
      if sum = target then
        (start, ``end``)
      else if sum > target then
        let newSum = sum - input.[start]
        findRangeSum (start+1) ``end`` newSum
      else
        let newSum = sum + input.[``end``+1]
        findRangeSum start (``end``+1) newSum

    let (s,e) = findRangeSum 0 0 (input.[0])
    let range =
      input.[s..e]
      |> Array.sort
    
    sw.Stop()
    sprintf "Results: %d; elapsed: %O" ((range |> Array.head) + (range |> Array.last)) sw.Elapsed
  | _ -> "Invalid Part input"