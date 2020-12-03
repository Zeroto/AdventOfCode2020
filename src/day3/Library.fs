module Day3

open System

type Tile =
  | Open
  | Tree

let charToTile c =
  match c with
  | '.' -> Open
  | '#' -> Tree
  | x -> failwithf "Invalid character in input: '%c'" x

let parseInput (s: string) =
  let map =
    s.Split([|'\n';'\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (Seq.map charToTile >> Seq.toArray)
  let width = map.[0].Length
  let height = map.Length
  (width, height, map)

let sampleTile width height (tiles: Tile array array) (x,y) =
  let xm = x % width
  let ym = y % height
  tiles.[ym].[xm]

let getTreeCount sampler positions  =
  positions
  |> List.map sampler
  |> List.filter ((=) Tree)
  |> List.length

let main (argv: string array) =
  let width, height, map =
    IO.File.ReadAllText argv.[1]
    |> parseInput

  let sample = sampleTile width height map
  let getTreeCount = getTreeCount sample

  match argv.[0] with
  | "a" ->
    let positions =
      [1..height-1]
      |> List.map (fun y -> (y*3, y))

    let result = getTreeCount positions
    sprintf "%d" result
  | "b" ->
    let slopes =
      [ (1,1)
        (3,1)
        (5,1)
        (7,1)
        (1,2)
      ]
      
    let result =
      slopes
      |> List.map (fun (xs,ys) -> [0..ys..height-1] |> List.mapi (fun i y -> (i*xs, y)) |> getTreeCount |> int64)
      |> List.reduce (*)

    sprintf "%d" result
      
  | _ -> "Invalid input"