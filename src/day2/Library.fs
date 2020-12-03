module Day2

open System

type Condition = int * int * char

type Password = {
  condition: Condition
  value: string
}

let parseConditionValues (s: string) =
  let t = s.Split('-')
  (int t.[0], int t.[1])

let parseCondition (s: string): Condition =
  let t = s.Split(' ')
  let cv = parseConditionValues t.[0]
  (fst cv, snd cv, char t.[1])

let parseInput (s: string) =
  s.Split('\n')
  |> Array.map
    (fun l ->
      let t = l.Split(':')
      let c = parseCondition t.[0]
      let v = t.[1].Trim()
      { condition = c; value = v}
    )

let checkPasswordA (p: Password) =
  let min, max, charNeeded = p.condition
  let length =
    p.value
    |> Seq.filter (fun c -> c = charNeeded)
    |> Seq.length
  length >= min && length <= max

let checkPasswordB (p: Password) =
  let pos1, pos2, charNeeded = p.condition
  p.value.[pos1-1] = charNeeded && p.value.[pos2-1] <> charNeeded
  || p.value.[pos1-1] <> charNeeded && p.value.[pos2-1] = charNeeded

let main (argv: string array) =
  let input =
    IO.File.ReadAllText argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let sw = Diagnostics.Stopwatch.StartNew()
    let correctPasswords =
      input
      |> Array.filter checkPasswordA
      |> Array.length
    sw.Stop()

    sprintf "Total count correct: %A; elapsed: %A" correctPasswords sw.Elapsed
  | "b" ->
    let sw = Diagnostics.Stopwatch.StartNew()
    let correctPasswords =
      input
      |> Array.filter checkPasswordB
      |> Array.length
    sw.Stop()
    
    sprintf "Total count correct: %A; elapsed: %A" correctPasswords sw.Elapsed
  | _ -> "Incorrect part input"