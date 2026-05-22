module IssuesTest

open System
open Expecto
open FsYaml
open FsYaml.NativeTypes

// Record where declaration order differs from alphabetical order
type OutOfAlphaOrderRecord = { Zebra: int; Alpaca: string; Mango: bool }

[<Tests>]
let fieldOrderTests =
  testList "Field order" [
    testCase "dump preserves record field declaration order not alphabetical" <| fun () ->
      let yaml = Yaml.dump { Zebra = 1; Alpaca = "a"; Mango = true }
      let fieldNames =
        yaml.Split('\n')
        |> Array.filter (fun l -> l.Contains(':') && l.Trim() <> "")
        |> Array.map (fun l -> l.Trim().Split(':').[0])
      Expect.equal (Array.toList fieldNames) [ "Zebra"; "Alpaca"; "Mango" ]
        "fields should appear in declaration order (Zebra, Alpaca, Mango), not alphabetical (Alpaca, Mango, Zebra)"
  ]

[<Tests>]
let tryLoadTests =
  testList "tryLoad" [
    testCase "tryLoad does not swallow non-FsYamlException" <| fun () ->
      // Exceptions thrown in Accept are NOT wrapped by Native.fs (they occur outside its try block).
      // Before the fix, tryLoad's bare `_ -> None` swallowed them silently.
      let buggyDef = {
        Accept = fun t ->
          if t = typeof<int> then raise (InvalidOperationException("deliberate bug in Accept"))
          else false
        Construct = fun _ _ _ -> box 0
        Represent = fun _ _ _ -> FsYaml.RepresentationTypes.Null None
      }
      Expect.throwsT<InvalidOperationException>
        (fun () -> Yaml.tryLoadWith<int> [ buggyDef ] "1" |> ignore)
        "non-FsYamlException should propagate out of tryLoad, not be silently swallowed"
  ]
