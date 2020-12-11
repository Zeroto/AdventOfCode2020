﻿module Day11
open System.IO
open System

type Tile =
  | Empty
  | FullSeat
  | EmptySeat

let parseInput =
  Array.map (fun l ->
    l
    |> Seq.map (fun c ->
      match c with
      | '.' -> Empty
      | 'L' -> EmptySeat
      | '#' -> FullSeat
      | _ -> failwithf "Invalid character in input: %c" c
    )
    |> Seq.toList
  )
  >> Array.toList

let vectors = [
  (-1,-1);(0,-1);(1,-1)
  (-1, 0);       (1, 0)
  (-1, 1);(0, 1);(1, 1)
]

let iterateBoardA width height (board: Tile list list) =
  board
  |> List.mapi (fun y line -> 
    line |> List.mapi (fun x tile -> 
      let otherPositions =
        vectors
        |> List.map (fun (xd, yd) -> (xd + x, yd + y))
        |> List.filter (fun (x,y) -> x >= 0 && x < width && y >= 0 && y < height)

      let seatsTakenNearby =
        otherPositions
        |> List.filter (fun (x,y) -> board.[y].[x] = FullSeat)
        |> List.length

      if tile = EmptySeat && seatsTakenNearby = 0 then
        FullSeat
      else if tile = FullSeat && seatsTakenNearby >= 4 then
        EmptySeat
      else tile
    ) 
  )

let iterateBoardB width height (board: Tile list list) =
  let rec walkUntilSeat x y (xd,yd) =
    let x = x + xd
    let y = y + yd
    if x >= 0 && x < width && y >= 0 && y < height then
      let tile = board.[y].[x]
      if tile = Empty then
        walkUntilSeat x y (xd,yd)
      else
        tile
    else // outside map
      EmptySeat

  board
  |> List.mapi (fun y line -> 
    line |> List.mapi (fun x tile -> 
      let seatsTakenNearby =
        vectors
        |> List.map (walkUntilSeat x y)
        |> List.filter (fun tile -> tile = FullSeat)
        |> List.length

      if tile = EmptySeat && seatsTakenNearby = 0 then
        FullSeat
      else if tile = FullSeat && seatsTakenNearby >= 5 then
        EmptySeat
      else tile
    ) 
  )

let rec iterateUntilUnchanged iterator board =
  let newBoard = iterator board
  if board = newBoard then
    board
  else
    iterateUntilUnchanged iterator newBoard

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  let width = input.[0].Length
  let height = input.Length

  match argv.[0] with
  | "a" ->
    let iterateBoard = iterateBoardA width height
    let finalBoard = iterateUntilUnchanged iterateBoard input
    let result = finalBoard |> List.sumBy (List.filter ((=) FullSeat) >> List.length)
    sprintf "Result: %d" result
  | "b" ->
    let iterateBoard = iterateBoardB width height
    let finalBoard = iterateUntilUnchanged iterateBoard input
    let result = finalBoard |> List.sumBy (List.filter ((=) FullSeat) >> List.length)
    sprintf "Result: %d" result
  | _ -> "Invalid Part input"