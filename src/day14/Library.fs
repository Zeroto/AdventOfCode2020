module Day14
open System.IO
open System

type Op =
  | Mask of string
  | Mem of address: uint64 * value: uint64

let parseInput =
  Array.map (fun (l: String) ->
    let parts = l.Split([|" = "|], StringSplitOptions.RemoveEmptyEntries)
    if parts.[0] = "mask" then
      Mask parts.[1]
    else
      let address = parts.[0].[4..(parts.[0].Length-2)]
      Mem (uint64 address, uint64 parts.[1])
  )

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let _,_,resultMemory =
      ((UInt64.MaxValue, uint64 0, Map.empty), input)
      ||> Array.fold (fun (andMask, orMask, memory) op ->
        match op with
        | Mask v ->
          let (andMask, orMask, _) =
            ((uint64 0, uint64 0, 0), v |> Seq.rev)
            ||> Seq.fold (fun (am, om,i) c ->
              match c with
              | 'X' -> (am + ((uint64 1) <<< i), om, i+1)
              | '1' -> (am, om + ((uint64 1) <<< i), i+1)
              | '0' -> (am, om, i+1)
              | _ -> failwithf "unexpected character in mask %c" c
            )
          (andMask, orMask, memory)
        | Mem (address, v) ->
          (andMask, orMask, memory |> Map.add address (v &&& andMask ||| orMask))
      )
    let result = resultMemory |> Seq.sumBy (fun kv -> kv.Value)
    sprintf "Result: %d" result
  | "b" ->
    let getAddressesWithMask mask address =
      (([0UL], 0), mask |> Seq.rev)
      ||> Seq.fold (fun (partials, i) c ->
        match c with
        | '1' -> partials |> List.map (fun x -> x + (1UL <<< i)), i+1
        | '0' -> (partials |> List.map (fun x -> x + (address &&& (1UL <<< i))), i+1)
        | 'X' -> ((partials |> List.map (fun x -> x + (1UL <<< i))) @ partials), i+1
        | _ -> failwithf "Invalid character in mask %c" c
      ) |> fst

    let _,resultMemory =
      (("", Map.empty), input)
      ||> Array.fold (fun (mask, memory) op ->
        match op with
        | Mask v ->
          (v, memory)
        | Mem (address, v) ->
          let addresses = getAddressesWithMask mask address
          let newMemory = 
            (memory, addresses)
            ||> List.fold (fun m a -> m |> Map.add a v)
          (mask, newMemory)
      )
    let result = resultMemory |> Seq.sumBy (fun kv -> kv.Value)
    sprintf "Result: %d" result
  | _ -> "Invalid Part input"