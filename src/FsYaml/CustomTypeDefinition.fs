/// <summary>
/// Provides a helper function to create a user-defined <see cref="FsYaml.NativeTypes.TypeDefinition" />.
/// </summary>
module FsYaml.CustomTypeDefinition

open System
open FsYaml.Utility
open FsYaml.RepresentationTypes

/// <summary>
/// The Constructor raises an exception stating that it expected <c>YamlObject.Scalar</c> but not.
/// </summary>
/// <param name="t">The type you tried to convert</param>
/// <param name="actual">Actual Yaml</param>
let mustBeScalar t actual = FsYamlException.WithYaml(actual, Resources.getString "mustBeScalar", Type.print t, YamlObject.nodeTypeName actual)
/// <summary>
/// The Constructor raises an exception stating that it expected <c> YamlObject.Sequence</c> but did not.
/// </summary>
/// <param name="t">The type you tried to convert</param>
/// <param name="actual">Actual Yaml</param>
let mustBeSequence t actual = FsYamlException.WithYaml(actual, Resources.getString "mustBeSequence", Type.print t, YamlObject.nodeTypeName actual)
/// <summary>
/// The Constructor raises an exception stating that <c>YamlObject.Mapping</c> was expected but not.
/// </summary>
/// <param name="t">The type you tried to convert</param>
/// <param name="actual">Actual Yaml</param>
let mustBeMapping t actual = FsYamlException.WithYaml(actual, Resources.getString "mustBeMapping", Type.print t, YamlObject.nodeTypeName actual)

/// <summary>
/// Create an object from <c>YamlObject.Scalar</ c>.
/// </summary>
/// <param name="f">Function to convert the value of <c>YamlObject.Scalar </c> to an object</param>
let constructFromScalar f = fun construct' t yaml ->
  match yaml with
  | Scalar (s, _) -> Scalar.value s |> f |> box
  | otherwise -> raise (mustBeScalar t otherwise)
/// <summary>
/// Converts an object to a plain <c> YamlObject.Scalar </c>.
/// </summary>
/// <param name="f">A function that transforms an object into the value of <c>YamlObject.Scalar </c></param>
let representAsPlain f = fun represent t obj -> Scalar (Plain (f obj), None)
/// <summary>
/// Converts an object to a NonPlain <c>YamlObject.Scalar</ c>.
/// </summary>
/// <param name="f">A function that transforms an object into the value of <c> YamlObject.Scalar </c></param>
let representAsNonPlain f = fun represent t obj -> Scalar (NonPlain (f obj), None)

/// <summary>
/// Convert from Seq type object to <c>YamlObject.Sequence</c>.
/// </summary>
let representSeqAsSequence = fun represent t obj ->
  let elementType = RuntimeSeq.elementType t
  let values = RuntimeSeq.map (represent elementType) t obj |> Seq.toList
  Sequence (values, None)

/// Provides a Constructor / Representer with a potentially null value.
module MaybeNull =
  /// <summary>
  /// Create an object from <c>YamlObject.Scalar</ c>. Returns null if the value is <c>YamlObject.Null</ c>.
  /// </summary>
  /// <param name="f">Function that converts the value of<c>YamlObject.Scalar</c></param>
  let constructFromScalar f = fun construct' t yaml ->
    match yaml with
    | Scalar (s, _) -> Scalar.value s |> f |> box
    | Null _ -> null
    | otherwise -> raise (mustBeScalar t otherwise)
  /// <summary>
  /// Converts an object to a plain <c>YamlObject.Scalar</c>. If the object is null, <c>YamlObject.Null</c> is returned.
  /// </summary>
  /// <param name="f">A function that transforms an object into the value of <c> YamlObject.Scalar</c></param>
  let representAsPlain f = fun represent t obj ->
    match obj with
    | null -> Null None
    | _ -> Scalar (Plain (f obj), None)
  /// <summary>
  /// Converts an object to a NonPlain <c>YamlObject.Scalar</ c>. If the object is null, <c>YamlObject.Null</ c> is returned.
  /// </summary>
  /// <param name="f">A function that transforms an object into the value of <c>YamlObject.Scalar</c></param>
  let representAsNonPlain f = fun represent t obj ->
    match obj with
    | null -> Null None
    | _ -> Scalar (NonPlain (f obj), None)

/// <summary>
/// Returns whether the specified type matches the generic type definition.
/// </summary>
/// <param name="genericTypeDef">Expected generic type definition</param>
/// <param name="x">Type to test</param>
let isGenericTypeDef genericTypeDef (x: Type) = x.IsGenericType && x.GetGenericTypeDefinition() = genericTypeDef
