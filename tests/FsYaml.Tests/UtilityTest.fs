module UtilityTest

open System
open Expecto
open FsYaml.Utility
open Microsoft.FSharp.Reflection

type GenericType<'a> = GeneticType of 'a

type TestAttr() = inherit Attribute()
[<TestAttr>] type WithTestAttr() = class end
type WithoutTestAttr() = class end
type UtilityRecord = { Field: int }
type UtilityUnion = UtilityCase

[<Tests>]
let tests =
  testList "Utility" [

    testList "Type.print" [
      for t, expected in [
        typeof<int>,               "int"
        typeof<DateTime>,          "DateTime"
        typeof<int * string>,      "int * string"
        typeof<int * (int * int)>, "int * (int * int)"
        typeof<int list>,          "int list"
        typeof<int[] list>,        "int[] list"
        typeof<(int * int) list>,  "(int * int) list"
        typeof<int * int list>,    "int * int list"
        typeof<Map<string, int>>,  "Map<string, int>"
        typeof<GenericType<int>>,  "GenericType<int>"
        typeof<int[]>,             "int[]"
      ] do
        yield testCase expected <| fun () ->
          Expect.equal (Type.print t) expected ""
    ]

    testList "Attribute.tryGetCustomAttribute" [
      testCase "returns Some when attribute present" <| fun () ->
        Expect.isSome (Attribute.tryGetCustomAttribute<TestAttr> typeof<WithTestAttr>) ""
      testCase "returns None when attribute absent" <| fun () ->
        Expect.isNone (Attribute.tryGetCustomAttribute<TestAttr> typeof<WithoutTestAttr>) ""
    ]

    testCase "PropertyInfo.print formats as Type.Field" <| fun () ->
      let field = FSharpType.GetRecordFields(typeof<UtilityRecord>).[0]
      Expect.equal (PropertyInfo.print field) "UtilityRecord.Field" ""

    testCase "Union.printCase formats as Type.Case" <| fun () ->
      let case = FSharpType.GetUnionCases(typeof<UtilityUnion>).[0]
      Expect.equal (Union.printCase case) "UtilityUnion.UtilityCase" ""

    testList "ObjectElementSeq" [
      testCase "cast obj seq to int seq" <| fun () ->
        let xs = seq { 1..3 } |> Seq.map box
        Expect.sequenceEqual (unbox<int seq> (ObjectElementSeq.cast typeof<int> xs)) (seq { 1..3 }) ""
      testCase "cast empty obj seq to int seq" <| fun () ->
        Expect.sequenceEqual (unbox<int seq> (ObjectElementSeq.cast typeof<int> (Seq.empty<obj>))) Seq.empty<int> ""
      testCase "convert obj seq to int list" <| fun () ->
        let xs = seq { 1..3 } |> Seq.map box
        Expect.equal (unbox<int list> (ObjectElementSeq.toList typeof<int> xs)) [ 1..3 ] ""
      testCase "convert empty obj seq to int list" <| fun () ->
        Expect.equal (unbox<int list> (ObjectElementSeq.toList typeof<int> (Seq.empty<obj>))) ([]: int list) ""
      testCase "convert obj pairs to Map<string, int>" <| fun () ->
        let xs = [ "1", 2; "3", 4; "4", 5 ] |> List.map (fun (k, v) -> box k, box v)
        Expect.equal (unbox<Map<string, int>> (ObjectElementSeq.toMap typeof<string> typeof<int> xs)) (Map.ofList [ "1", 2; "3", 4; "4", 5 ]) ""
      testCase "convert empty obj pairs to Map<string, int>" <| fun () ->
        Expect.equal (unbox<Map<string, int>> (ObjectElementSeq.toMap typeof<string> typeof<int> ([] : (obj * obj) list))) Map.empty<string, int> ""
      testCase "convert obj seq to int[]" <| fun () ->
        let xs = seq { 1..3 } |> Seq.map box
        Expect.equal (unbox<int[]> (ObjectElementSeq.toArray typeof<int> xs)) [| 1..3 |] ""
      testCase "convert empty obj seq to int[]" <| fun () ->
        Expect.equal (unbox<int[]> (ObjectElementSeq.toArray typeof<int> (Seq.empty<obj>))) Array.empty<int> ""
    ]

    testList "RuntimeSeq.map" [
      testCase "map over list" <| fun () ->
        Expect.sequenceEqual (RuntimeSeq.map (fun x -> string x) typeof<int list> (box [ 1; 2; 3 ])) [ "1"; "2"; "3" ] ""
      testCase "map over array" <| fun () ->
        Expect.sequenceEqual (RuntimeSeq.map (fun x -> string x) typeof<int[]> (box [| 1; 2; 3 |])) [ "1"; "2"; "3" ] ""
      testCase "map over seq" <| fun () ->
        Expect.sequenceEqual (RuntimeSeq.map (fun x -> string x) typeof<int seq> (box (seq { 1..3 }))) [ "1"; "2"; "3" ] ""
    ]

    testCase "RuntimeMap.toSeq converts map to key-value pairs" <| fun () ->
      let map = Map.ofList [ "a", 1; "b", 2 ]
      Expect.sequenceEqual (RuntimeMap.toSeq typeof<Map<string, int>> map) [ box "a", box 1; box "b", box 2 ] ""
  ]
