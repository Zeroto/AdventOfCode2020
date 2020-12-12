module Day12
open System.IO
open System

[<Measure>] type m
[<Measure>] type d

type Ship = {
  facing: int<d>
  x: int<m>
  y: int<m>
  waypoint: int<m> * int<m>
}

let initialShip = {
  facing = 90<d>
  x = 0<m>
  y = 0<m>
  waypoint = 10<m>,1<m>
}

type Instruction =
  | N of int<m>
  | E of int<m>
  | S of int<m>
  | W of int<m>
  | L of int<d>
  | R of int<d>
  | F of int<m>

let parseInstruction (s: string) =
  let instr = s.[0]
  let value = int (s.Substring(1))
  match instr with
  | 'N' -> N (value * 1<m>)
  | 'E' -> E (value * 1<m>)
  | 'S' -> S (value * 1<m>)
  | 'W' -> W (value * 1<m>)
  | 'L' -> L (value * 1<d>)
  | 'R' -> R (value * 1<d>)
  | 'F' -> F (value * 1<m>)
  | _ -> failwithf "Invalid instruction %s" s

let parseInput = Array.map parseInstruction

let rotatePointAroundOrigin (x,y) d =
  match d with
  | 0<d> -> (x, y)
  | 90<d> -> (y, -x)
  | 180<d> -> (-x, -y)
  | 270<d> -> (-y, x)
  | _ -> failwithf "Invalid direction %d" d

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let moveShip ship instr =
      match instr with
      | N u -> {ship with y = ship.y + u}
      | E u -> {ship with x = ship.x + u}
      | S u -> {ship with y = ship.y - u}
      | W u -> {ship with x = ship.x - u}
      | L u -> {ship with facing = if ship.facing - u < 0<d> then ship.facing - u + 360<d> else ship.facing - u}
      | R u -> {ship with facing = (ship.facing + u) % 360<d>}
      | F u ->
        match ship.facing with
        | 0<d> ->   {ship with y = ship.y + u}
        | 90<d> ->  {ship with x = ship.x + u}
        | 180<d> -> {ship with y = ship.y - u}
        | 270<d> -> {ship with x = ship.x - u}
        | _ -> failwithf "Invalid facing direction %d" ship.facing
    let finalShip =
      (initialShip, input)
      ||> Array.fold moveShip

    let result = Math.Abs(int finalShip.x) + Math.Abs(int finalShip.y)
    sprintf "Result: %d" result
  | "b" ->
    let moveShip ship instr =
      printfn "%A; %A" ship instr
      match instr with
      | N u -> {ship with waypoint = (fst ship.waypoint, (snd ship.waypoint) + u)}
      | E u -> {ship with waypoint = (fst ship.waypoint + u, snd ship.waypoint)}
      | S u -> {ship with waypoint = (fst ship.waypoint, (snd ship.waypoint) - u)}
      | W u -> {ship with waypoint = (fst ship.waypoint - u, snd ship.waypoint)}
      | L u -> {ship with waypoint = rotatePointAroundOrigin ship.waypoint (360<d> - u)}
      | R u -> {ship with waypoint = rotatePointAroundOrigin ship.waypoint u}
      | F u -> {ship with x = ship.x + (fst ship.waypoint * int u); y = ship.y + (snd ship.waypoint * int u)}
    let finalShip =
      (initialShip, input)
      ||> Array.fold (moveShip)

    let result = Math.Abs(int finalShip.x) + Math.Abs(int finalShip.y)
    sprintf "Result: %d" result
  | _ -> "Invalid Part input"