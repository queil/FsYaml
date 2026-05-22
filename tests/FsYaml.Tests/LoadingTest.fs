module LoadingTest

open System
open Expecto
open FsYaml
open FsYaml.RepresentationTypes
open FsYaml.NativeTypes
open FsYaml.CustomTypeDefinition

let rec clearPosition = function
  | Scalar (v, _) -> Scalar (v, None)
  | Sequence (v, _) -> Sequence (List.map clearPosition v, None)
  | Mapping (v, _) ->
    let mapping = v |> List.map (fun (k, v) -> clearPosition k, clearPosition v)
    Mapping (mapping, None)
  | Null _ -> Null None

let parse = Representation.parse >> clearPosition

type UnknownType() = class end

type OneCase = OneCase
type TwoCase = TwoCase_1 | TwoCase_2
type OneFieldCase = OneFieldCase of int
type TupleFieldCase = TupleFieldCase of (int * string)
type ListFieldCase = ListFieldCase of int list
type MapFieldCase = MapFieldCase of Map<string, int>
type ManyFieldsCase = ManyFieldsCase of int * string * string
type OneNamedFieldCase = OneNamedFieldCase of x: int
type ManyNamedFieldCase = ManyNamedFieldCase of x: int * y: string
type HalfNamedFieldCase =
  | HalfNamedFieldCaseA of x: int * int
  | HalfNamedFieldCaseB of int * y: int
[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type UseNullAsTrueValueCase = NullCase | ValueCase1 of int | ValueCase2 of string

type LoadRecord = { A: int; B: string }
type WithDefaultMember = { A: string; B: int } with static member DefaultB = 3
type InvalidDefaultType = { A: string; B: int } with static member DefaultB = "3"
type WithOption = { A: int; B: int option }

type CustomType(x: int) =
  member val X = x
  override this.Equals(other) =
    match other with
    | :? CustomType as other -> this.X = other.X
    | _ -> false
  override this.GetHashCode() = this.X.GetHashCode()

let customConstructor = {
  Accept = ((=) typeof<CustomType>)
  Construct = fun construct' t yaml ->
    match yaml with
    | Scalar _ as x -> CustomType(construct' typeof<int> x :?> int) |> box
    | _ -> null
  Represent = fun _ -> raise (NotImplementedException())
}

type SimpleUnion = Case1 | Case2 of int list

[<Tests>]
let representationTests =
  testList "Representation" [
    testList "can parse Plain scalars" [
      for input in [ "abc"; "1"; "3:4"; "3-4" ] do
        yield testCase input <| fun () ->
          Expect.equal (parse input) (Scalar (Plain input, None)) ""
    ]
    testList "can parse NonPlain scalars" [
      for input, expected in [ "\"abc\"", "abc"; "'abc'", "abc" ] do
        yield testCase input <| fun () ->
          Expect.equal (parse input) (Scalar (NonPlain expected, None)) ""
    ]
    testList "can parse Sequence" [
      let expected =
        Sequence ([ Scalar (Plain "abc", None); Scalar (Plain "def", None); Scalar (NonPlain "ghi", None) ], None)
      yield testCase "flow style" <| fun () ->
        Expect.equal (parse """[ abc, def, "ghi" ]""") expected ""
      yield testCase "block style" <| fun () ->
        Expect.equal (parse "- abc\n- def\n- \"ghi\"") expected ""
    ]
    testList "can parse Mapping" [
      let expected =
        Mapping ([ Scalar (Plain "abc", None), Scalar (Plain "def", None)
                   Scalar (Plain "ghi", None), Sequence ([ Scalar (Plain "jkf", None) ], None) ], None)
      yield testCase "flow style" <| fun () ->
        Expect.equal (parse "{ abc: def, ghi: [ jkf ] }") expected ""
      yield testCase "block style" <| fun () ->
        Expect.equal (parse "abc: def\nghi:\n     - jkf") expected ""
    ]
    testList "can parse Null" [
      let expected = Mapping ([ Scalar (Plain "abc", None), Null None ], None)
      for input in [ "abc: "; "abc: null"; "abc: ~" ] do
        yield testCase input <| fun () ->
          Expect.equal (parse input) expected ""
    ]
  ]

[<Tests>]
let loadTests =
  testList "Load" [
    testCase "unregistered type throws FsYamlException" <| fun () ->
      Expect.throwsT<FsYamlException> (fun () -> Yaml.load<UnknownType> "1" |> ignore) ""

    testCase "can load int" <| fun () ->
      Expect.equal (Yaml.load<int> "1") 1 ""

    testCase "invalid int throws FsYamlException" <| fun () ->
      Expect.throwsT<FsYamlException> (fun () -> Yaml.load<int> "not int" |> ignore) ""

    testCase "can load int64" <| fun () ->
      Expect.equal (Yaml.load<int64> "234") 234L ""

    testList "can load float" [
      for yaml, expected in [ "234.5", 234.5; ".inf", infinity; "+.inf", infinity; "-.inf", -infinity; ".nan", Double.NaN ] do
        yield testCase yaml <| fun () ->
          let actual = Yaml.load<float> yaml
          if Double.IsNaN expected then Expect.isTrue (Double.IsNaN actual) ""
          else Expect.equal actual expected ""
    ]

    testList "can load string" [
      for yaml, expected in [ "aaaaa", "aaaaa"; "''", ""; "~", null; "null", null; "'null'", "null" ] do
        yield testCase (sprintf "%A" yaml) <| fun () ->
          Expect.equal (Yaml.load<string> yaml) expected ""
    ]

    testList "can load bool" [
      for yaml, expected in [
        "true", true; "yes", true; "y", true; "on", true
        "FALSE", false; "NO", false; "N", false; "OFF", false
      ] do
        yield testCase yaml <| fun () ->
          Expect.equal (Yaml.load<bool> yaml) expected ""
    ]

    testCase "can load decimal" <| fun () ->
      Expect.equal (Yaml.load<decimal> "100.5") 100.5m ""

    testCase "can load DateTime" <| fun () ->
      let yaml = " 2015-2-5 22:00:01.222"
      Expect.equal (Yaml.load<DateTime> yaml) (DateTime.Parse(yaml)) ""

    testCase "can load TimeSpan" <| fun () ->
      let yaml = "01:22:33.444"
      Expect.equal (Yaml.load<TimeSpan> yaml) (TimeSpan.Parse(yaml)) ""

    testCase "can load tuple" <| fun () ->
      Expect.equal (Yaml.load<int * int * string * string> "[ 1, 2, 3, abc ]") (1, 2, "3", "abc") ""

    testCase "tuple with too few elements throws FsYamlException" <| fun () ->
      Expect.throwsT<FsYamlException> (fun () -> Yaml.load<int * int * int * int> "[ 1, 2, 3 ]" |> ignore) ""

    testCase "tuple with too many elements throws FsYamlException" <| fun () ->
      Expect.throwsT<FsYamlException> (fun () -> Yaml.load<int * int * int * int> "[ 1, 2, 3, 4, 5 ]" |> ignore) ""

    testCase "can load list" <| fun () ->
      Expect.equal (Yaml.load<string list> "[ a, b, c, d, e ]") [ "a"; "b"; "c"; "d"; "e" ] ""

    testCase "can load empty list" <| fun () ->
      Expect.equal (Yaml.load<string list> "[]") ([]: string list) ""

    testCase "can load set" <| fun () ->
      Expect.equal (Yaml.load<Set<int>> "[ 1, 2, 3 ]") (set [ 1; 2; 3 ]) ""

    testCase "can load map" <| fun () ->
      Expect.equal (Yaml.load<Map<string, int>> "{ a: 1, b: 2 }") (Map.ofList [ "a", 1; "b", 2 ]) ""

    testCase "can load empty map" <| fun () ->
      Expect.equal (Yaml.load<Map<string, int>> "{}") Map.empty<string, int> ""

    testCase "can load array" <| fun () ->
      Expect.equal (Yaml.load<int[]> "[ 1, 2, 3 ]") [| 1; 2; 3 |] ""

    testCase "can load empty array" <| fun () ->
      Expect.equal (Yaml.load<int[]> "[]") Array.empty<int> ""

    testCase "can load seq" <| fun () ->
      Expect.sequenceEqual (Yaml.load<seq<int>> "[ 1, 2, 3 ]") (seq { 1..3 }) ""

    testCase "can load empty seq" <| fun () ->
      Expect.sequenceEqual (Yaml.load<seq<int>> "[]") Seq.empty<int> ""
  ]

[<Tests>]
let loadUnionTests =
  testList "Load union" [
    testCase "single case no fields" <| fun () ->
      Expect.equal (Yaml.load<OneCase> "OneCase") OneCase ""

    testCase "two cases no fields" <| fun () ->
      Expect.equal (Yaml.load<TwoCase> "TwoCase_2") TwoCase_2 ""

    testCase "one unnamed field" <| fun () ->
      Expect.equal (Yaml.load<OneFieldCase> "OneFieldCase: 1") (OneFieldCase 1) ""

    testCase "tuple field" <| fun () ->
      Expect.equal (Yaml.load<TupleFieldCase> "TupleFieldCase: [ 1, a ]") (TupleFieldCase (1, "a")) ""

    testCase "list field" <| fun () ->
      Expect.equal (Yaml.load<ListFieldCase> "ListFieldCase: [ 1, 2, 3 ]") (ListFieldCase [ 1; 2; 3 ]) ""

    testCase "map field" <| fun () ->
      Expect.equal (Yaml.load<MapFieldCase> "MapFieldCase: { a: 1, b: 2 }") (MapFieldCase (Map.ofList [ "a", 1; "b", 2 ])) ""

    testCase "multiple unnamed fields" <| fun () ->
      Expect.equal (Yaml.load<ManyFieldsCase> "ManyFieldsCase: [ 1, a, b ]") (ManyFieldsCase (1, "a", "b")) ""

    testCase "one named field" <| fun () ->
      Expect.equal (Yaml.load<OneNamedFieldCase> "OneNamedFieldCase: { x: 1 }") (OneNamedFieldCase (x = 1)) ""

    testCase "multiple named fields" <| fun () ->
      Expect.equal (Yaml.load<ManyNamedFieldCase> "ManyNamedFieldCase: { x: 1, y: a }") (ManyNamedFieldCase (x = 1, y = "a")) ""

    testCase "implicit single field name" <| fun () ->
      Expect.equal (Yaml.load<OneFieldCase> "OneFieldCase: { Item: 1 }") (OneFieldCase 1) ""

    testCase "implicit multiple field names" <| fun () ->
      Expect.equal (Yaml.load<ManyFieldsCase> "ManyFieldsCase: { Item1: 1, Item2: a, Item3: b }") (ManyFieldsCase (1, "a", "b")) ""

    testCase "half-named fields - first named" <| fun () ->
      Expect.equal (Yaml.load<HalfNamedFieldCase> "HalfNamedFieldCaseA: { x: 1, Item2: 2 }") (HalfNamedFieldCaseA (1, 2)) ""

    testCase "half-named fields - second named" <| fun () ->
      Expect.equal (Yaml.load<HalfNamedFieldCase> "HalfNamedFieldCaseB: { Item1: 1, y: 2 }") (HalfNamedFieldCaseB (1, 2)) ""

    testCase "UseNullAsTrueValue - null case" <| fun () ->
      Expect.equal (Yaml.load<UseNullAsTrueValueCase> "NullCase") NullCase ""

    testCase "UseNullAsTrueValue - value case" <| fun () ->
      Expect.equal (Yaml.load<UseNullAsTrueValueCase> "ValueCase2: a") (ValueCase2 "a") ""

    testList "Option.None" [
      for yaml in [ "~"; "null"; "None" ] do
        yield testCase yaml <| fun () ->
          Expect.equal (Yaml.load<int option> yaml) None ""
    ]

    testList "Option.Some" [
      for yaml in [ "Some: 1"; "1" ] do
        yield testCase yaml <| fun () ->
          Expect.equal (Yaml.load<int option> yaml) (Some 1) ""
    ]
  ]

[<Tests>]
let loadRecordTests =
  testList "Load record" [
    testCase "can load record" <| fun () ->
      Expect.equal (Yaml.load<LoadRecord> "{ A: 123, B: abc }") { A = 123; B = "abc" } ""

    testCase "missing required field throws FsYamlException" <| fun () ->
      Expect.throwsT<FsYamlException> (fun () -> Yaml.load<LoadRecord> "{ A: 123 }" |> ignore) ""

    testCase "static default value fills in missing field" <| fun () ->
      Expect.equal (Yaml.load<WithDefaultMember> "{ A: abc }") { A = "abc"; B = 3 } ""

    testCase "static default value with wrong type throws FsYamlException" <| fun () ->
      Expect.throwsT<FsYamlException> (fun () -> Yaml.load<InvalidDefaultType> "{ A: abc }" |> ignore) ""

    testCase "omitted option field defaults to None" <| fun () ->
      Expect.equal (Yaml.load<WithOption> "{ A: 1 }") { A = 1; B = None } ""
  ]

[<Tests>]
let loadCustomTypeTests =
  testList "Load custom type" [
    testCase "can load user-defined type with custom constructor" <| fun () ->
      Expect.equal (Yaml.loadWith<CustomType> [ customConstructor ] "1") (CustomType 1) ""
  ]

[<Tests>]
let loadComplexTypeTests =
  testList "Load complex type" [
    testCase "can load list of unions" <| fun () ->
      let yaml = "- Case1\n- Case2: [ 42 ]"
      Expect.equal (Yaml.load<SimpleUnion list> yaml) [ Case1; Case2 [ 42 ] ] ""
  ]

[<Tests>]
let loadUntypedTests =
  testList "Load untyped" [
    testCase "can load record by Type reference" <| fun () ->
      let actual = Yaml.loadUntyped typeof<LoadRecord> "{ A: 123, B: abc }" |> unbox<LoadRecord>
      Expect.equal actual { A = 123; B = "abc" } ""
  ]
