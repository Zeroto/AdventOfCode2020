module Day7

open System
open System.IO
open System.Text.RegularExpressions

type Rule = {
  identifier: string
  contents: (int * string) seq
}

let inputRegex = Regex("""(?'identifier'\S+ \S+) bag[s]? contain (?:no other bags|(?:(?'value'\d+) (?'name'\S+ \S+) bag[s]?(?:, )?)+)\.$""")

let parseLine (s: string) =
  inputRegex.Match s
  |> fun m ->
    let identifier = m.Groups.["identifier"].Value
    let values = m.Groups.["value"].Captures |> Seq.cast<Capture> |> Seq.map (fun c -> int c.Value)
    let names = m.Groups.["name"].Captures |> Seq.cast<Capture> |> Seq.map (fun c -> c.Value)

    {
      identifier = identifier
      contents = Seq.zip values names
    }

let parseInput = Array.map parseLine

let main (argv: string array) =
  let input =
    File.ReadAllLines argv.[1]
    |> parseInput

  //printfn "%A" (input |> Array.map (sprintf "%A"))

  match argv.[0] with
  | "a" ->
    let rec findContaining (found: Set<string>) s =
      let containingBags =
        input
        |> Array.filter (fun r -> found |> Set.contains r.identifier |> not && r.contents |> Seq.exists (fun (_,name) -> name = s))
        |> Array.map (fun r -> r.identifier)
      if Seq.isEmpty containingBags then
        found
      else
        let newFound = found |> Set.union (Set.ofArray containingBags)
        (newFound, containingBags)
        ||> Seq.fold (fun f s -> f |> Set.union (findContaining newFound s))
    
    let result = findContaining Set.empty "shiny gold"
    sprintf "%d" (result |> Set.count)
  | "b" ->
    let shinyBagRule = input |> Array.find (fun r -> r.identifier = "shiny gold")
    let rec unfold (state: seq<int*string>) =
      (0, state)
      ||> Seq.fold 
        (fun s (times, name) ->
          match input |> Array.tryFind (fun r -> r.identifier = name) with
          | Some r ->
            let v = unfold r.contents
            s + (times * (v+1))
          | None -> 0)

    let result = unfold shinyBagRule.contents
    sprintf "%d" (result)
  | _ -> "Invalid Part input"