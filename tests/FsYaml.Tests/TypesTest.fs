module TypesTest

open System.Collections.Generic
open Expecto
open FsYaml.RepresentationTypes

[<Tests>]
let tests =
  testList "Types" [
    testCase "Mapping.find throws KeyNotFoundException when key not found" <| fun () ->
      let m = Map.ofList [ Scalar (Plain "1", None), Scalar (Plain "2", None) ]
      Expect.throwsT<KeyNotFoundException> (fun () -> Mapping.find "3" m |> ignore) ""
  ]
