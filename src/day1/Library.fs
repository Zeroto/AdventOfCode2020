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
  let count = Array.length input  

  match argv.[0] with
  | "a" ->
    let mutable i = 0
    let mutable pending = true
    let mutable result = 0
    while pending do
      let mutable i2 = i+1;
      while pending && i2 < count do
        if input.[i] + input.[i2] = 2020 then
          pending <- false
          result <- input.[i] * input.[i2]
        else
          i2 <- i2 + 1
      i <- i+1
    string result
  | "b" ->
    let sw = Diagnostics.Stopwatch.StartNew();
    let mutable i = 0
    let mutable pending = true
    let mutable result = 0
    while pending do
      let mutable i2 = i+1;
      while pending && i2 < count do
        let t = input.[i] + input.[i2]
        if t < 2020 then
          let mutable i3 = i2+1;
          while pending && i3 < count do
            if t + input.[i3] = 2020 then
              pending <- false
              result <- input.[i] * input.[i2] * input.[i3]
            else
              i3 <- i3 + 1
        i2 <- i2 + 1  
      i <- i+1
    sw.Stop()
    printfn "elapsed: %O" sw.Elapsed
    string result
  | _ -> null