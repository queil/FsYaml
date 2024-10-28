namespace FsYaml

open System

/// Provides the type of Representation layer in Yaml.
module RepresentationTypes =
  /// Scalar Node value
  type Scalar =
    /// Plain format Scalar
    | Plain of string
    /// Scalar other than Plain format
    | NonPlain of string

  /// It is a module that operates Scalar.
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Scalar =
    /// Gets the value of Scalar.
    let value = function
      | Plain x -> x
      | NonPlain x -> x

  /// Position in Yaml string
  type Position = { Line: int64; Column: int64; }

  /// Represents Yaml.
  type YamlObject =
    /// Scalar node
    | Scalar of Scalar * Position option
    /// Sequence node
    | Sequence of YamlObject list * Position option
    /// Mapping node
    | Mapping of Map<YamlObject, YamlObject> * Position option
    /// Null value that specializes in Scalar
    | Null of Position option

  /// A module that operates YamlObject.
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module YamlObject =
    open Microsoft.FSharp.Reflection

    /// Gets the position of the target object in the Yaml string.
    let position = function
      | Scalar (_, p) -> p
      | Sequence (_, p) -> p
      | Mapping (_, p) -> p
      | Null p -> p

    /// Get the name of the Node.
    let nodeTypeName (x: YamlObject) = let caseInfo, _ = FSharpValue.GetUnionFields(x, typeof<YamlObject>) in caseInfo.Name

  /// It is a module that operates Mapping.
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Mapping =
    let private pickF name = (fun k v ->
      match k with
      | Scalar (k, _) -> if Scalar.value k = name then Some v else None
      | _ -> None)
    /// Search for elements in Mapping. Returns None if the element does not exist.
    let tryFind key (mapping: Map<YamlObject, YamlObject>) = Map.tryPick (pickF key) mapping
    /// <summary>
    /// Search for elements in Mapping.
    /// </summary>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">If the key does not exist</exception>
    let find key (mapping: Map<YamlObject, YamlObject>) = Map.pick (pickF key) mapping

/// Provides a type of Native layer for Yaml.
module NativeTypes =
  open RepresentationTypes

  /// <summary>
  /// Convert from Yaml to an object of the type represented by <c>Type</c>.
  /// </summary>
  type Constructor = Type -> YamlObject -> obj
  /// <summary>
  /// Recursively convert from Yaml to an object. Used for generic type element and member conversion.
  /// </summary>
  type RecursiveConstructor = Constructor

  /// <summary>
  /// Converts an object of the type represented by <c>Type</c> to Yaml.
  /// </summary>
  type Representer = Type -> obj -> YamlObject
  /// <summary>
  /// Recursively convert objects to Yaml. Used for generic type element and member conversion.
  /// </summary>
  type RecursiveRepresenter = Representer

  /// Information about conversions between Yaml and objects.
  type TypeDefinition = {
    /// Returns whether the passed type is convertible.
    Accept: Type -> bool
    /// Convert from Yaml to an object.
    Construct: RecursiveConstructor -> Constructor
    /// Convert from an object to Yaml.
    Represent: RecursiveRepresenter -> Representer
  }
