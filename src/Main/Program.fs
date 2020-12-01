﻿open System

let noop argv =
  0

let days = [|
  ("day1", Day1.main, true)
  ("day2", noop, false)
  ("day3", noop, false)
  ("day4", noop, false)
  ("day5", noop, false)
  ("day6", noop, false)
  ("day7", noop, false)
  ("day8", noop, false)
  ("day9", noop, false)
  ("day10", noop, false)
  ("day11", noop, false)
  ("day12", noop, false)
  ("day13", noop, false)
  ("day14", noop, false)
  ("day15", noop, false)
  ("day16", noop, false)
  ("day17", noop, false)
  ("day18", noop, false)
  ("day19", noop, false)
  ("day20", noop, false)
  ("day21", noop, false)
  ("day22", noop, false)
  ("day23", noop, false)
  ("day24", noop, false)
  ("day25", noop, false)

|]

let runDay name argv =
  match days |> Array.tryFind (fun (x,_, enabled) -> x = name && enabled) with
  | Some (_, program, _) ->
    Console.Clear()
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    let result = program (argv)
    stopwatch.Stop();
    printfn "Time elapsed: %dms" stopwatch.ElapsedMilliseconds
    result
  | None ->
    printfn "Invalid day"
    0

let readInputArguments () =
  let rec read acc =
    let input = Console.ReadLine()
    if input = "" then
      acc |> Array.ofList
    else
      read (acc @ [input])

  read []

[<EntryPoint>]
let main argv =
  Console.Clear()
  Console.ResetColor()
  let day = argv |> Array.tryHead
  match day with
  | Some day ->
    runDay day (argv |> Array.tail)
  | None ->
    printfn "Select Day:\n"
    days |> Array.iteri (fun i (name, _, enabled) -> 
                          if enabled then Console.ForegroundColor <- ConsoleColor.Green else  Console.ForegroundColor <- ConsoleColor.Red
                          printfn "  %d -> %s" i name)
    Console.ResetColor()
    printf "\nEnter Input: "
    let success, input = Console.ReadLine() |> Int32.TryParse
    if success then
      match days |> Array.tryItem input with
        | Some (name, _, true) ->
          Console.Clear()
          printf "Selected %s\nEnter arguments(end with empty line):\n" name
          let arguments = readInputArguments ()
          runDay name arguments
        | _ ->
          printfn "invalid input"
          -1
    else
      printfn "invalid input"
      -1

