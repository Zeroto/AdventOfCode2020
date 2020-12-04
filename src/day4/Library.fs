module Day4

open System
open System.Text.RegularExpressions

type Passport = {
    byr: string option //(Birth Year)
    iyr: string option //(Issue Year)
    eyr: string option //(Expiration Year)
    hgt: string option //(Height)
    hcl: string option //(Hair Color)
    ecl: string option //(Eye Color)
    pid: string option //(Passport ID)
    cid: string option //(Country ID)
}

let emptyPassport = {
  byr = None
  iyr = None
  eyr = None
  hgt = None
  hcl = None
  ecl = None
  pid = None
  cid = None
}

let hairColorRegex = Regex("^#[0-9a-f]{6}$")
let eyeColors = ["amb";"blu";"brn";"gry";"grn";"hzl";"oth"]
let pidRegex = Regex("^[0-9]{9}$")

let parseEntry (e: string) =
  e.Split(':')
  |> fun t -> (t.[0], t.[1])

let parseInput (s: string array) = 
  (([],emptyPassport), s)
  ||> Array.fold (fun (ps, p) l ->
    if l = "" then
      ps @ [p], emptyPassport
    else
      let entries =
        l.Split(' ')
        |> Array.map parseEntry
      let newP =
        (p, entries)
        ||> Array.fold (fun s (k,v) -> 
          match k with
          | "byr" -> {s with byr = Some v}
          | "iyr" -> {s with iyr = Some v}
          | "eyr" -> {s with eyr = Some v}
          | "hgt" -> {s with hgt = Some v}
          | "hcl" -> {s with hcl = Some v}
          | "ecl" -> {s with ecl = Some v}
          | "pid" -> {s with pid = Some v}
          | "cid" -> {s with cid = Some v}
          | _ -> failwithf "unknown key in passport: %s" k
        )
      ps, newP
  )


let main (argv: string array) =
  let (input, _) =
    System.IO.File.ReadAllLines argv.[1]
    |> parseInput

  match argv.[0] with
  | "a" ->
    let isValidPassport (p: Passport) =
      p.byr.IsSome && p.iyr.IsSome && p.eyr.IsSome && p.hgt.IsSome
      && p.hcl.IsSome && p.ecl.IsSome && p.pid.IsSome
    let validPassports =
      input
      |> List.filter isValidPassport

    sprintf "Result: %d" (validPassports |> List.length)
  | "b" ->
    let validHeight (s: string) =
      let unit = s.Substring(s.Length-2)
      let success, value = Int32.TryParse (s.Substring(0, s.Length-2))
      match unit with
      | "cm" -> success && value >= 150 && value <= 193
      | "in" -> success && value >= 59 && value <= 76
      | _ -> false

    let isValidPassport (p: Passport) =
      match p with
      | { byr = Some byr 
          iyr = Some iyr
          eyr = Some eyr
          hgt = Some hgt
          hcl = Some hcl
          ecl = Some ecl
          pid = Some pid
          cid = _
        } when
          byr.Length = 4 && int byr >= 1920 && int byr <= 2002 &&
          iyr.Length = 4 && int iyr >= 2010 && int iyr <= 2020 &&
          eyr.Length = 4 && int eyr >= 2020 && int eyr <= 2030 &&
          validHeight hgt &&
          hairColorRegex.IsMatch hcl &&
          eyeColors |> List.contains ecl &&
          pidRegex.IsMatch pid
        -> true
      | _ -> false

    let validPassports =
      input
      |> List.filter isValidPassport

    validPassports
    |> List.map (sprintf "%A")
    |> String.concat "\n"
    |> (fun s -> System.IO.File.WriteAllText("output.log",s))
    sprintf "Result: %d" (validPassports |> List.length)