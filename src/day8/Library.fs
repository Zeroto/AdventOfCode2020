module Day8

open System.IO
open System

module Option =
  let defaultBind fn o =
    match o with
    | Some _ -> o
    | None -> fn ()

type Operands = 
  | Acc of int
  | Jmp of int
  | Nop of int

let parseLine (s: string) =
  let t = s.Split(' ')
  match t.[0] with
  | "acc" -> Acc (int t.[1])
  | "jmp" -> Jmp (int t.[1])
  | "nop" -> Nop (int t.[1])
  | _ -> failwithf "Unknown operand: %A" t

let parseInput = Array.map parseLine

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let rec eval visitedInstr acc ip =
      if visitedInstr |> Set.contains ip then
        acc
      else
        let nvi = visitedInstr |> Set.add ip
        let instruction = input.[ip]
        match instruction with
        | Acc i -> eval nvi (acc+i) (ip+1)
        | Jmp i -> eval nvi acc (ip+i)
        | Nop _ -> eval nvi acc (ip+1)

    let result = eval Set.empty 0 0
    sprintf "Result: %d" result
  | "b" ->
    let rec eval visitedInstr acc ip hasCorrected =
      if ip >= (input |> Array.length) then
        Some acc
      else if visitedInstr |> Set.contains ip then
        None
      else
        let nvi = visitedInstr |> Set.add ip
        let instruction = input.[ip]
        match instruction with
        | Acc i -> eval nvi (acc+i) (ip+1) hasCorrected
        | Jmp i ->
          if hasCorrected then
            eval nvi acc (ip+i) hasCorrected
          else
            eval nvi acc (ip+i) hasCorrected |> Option.defaultBind (fun () -> eval nvi acc (ip+1) true)
        | Nop i ->
          if hasCorrected then
            eval nvi acc (ip+1) hasCorrected
          else
            eval nvi acc (ip+1) hasCorrected |> Option.defaultBind (fun () -> eval nvi acc (ip+i) true)
    
    let result = eval Set.empty 0 0 false
    sprintf "Result: %A" result
  | _ -> "Invalid Part input"