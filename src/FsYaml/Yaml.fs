/// <summary>
/// Provides mutual conversion between objects and Yaml.
/// </summary>
/// <remarks>
/// The conversion supports the following types: To convert other types <see cref="FsYaml.NativeTypes.TypeDefinition" /> Implemented <c>loadWith</c>, <c>dumpWith</c>Pass to.<br />
/// <list type="bullet">
///   <item><description>int</description></item>
///   <item><description>int64</description></item>
///   <item><description>float</description></item>
///   <item><description>string</description></item>
///   <item><description>bool</description></item>
///   <item><description>decimal</description></item>
///   <item><description>System.DateTime</description></item>
///   <item><description>System.TimeSpan</description></item>
///   <item><description>Record type</description></item>
///   <item><description>Tuple</description></item>
///   <item><description>list&lt;'T&gt;</description></item>
///   <item><description>Map&lt;'Key, 'Value&gt;</description></item>
///   <item><description>set</description></item>
///   <item><description>seq&lt;'T&gt;</description></item>
///   <item><description>Option&lt;'T&gt;</description></item>
///   <item><description>Discriminated union</description></item>
/// </list>
/// </remarks>
module FsYaml.Yaml

/// <summary>
/// Load the Yaml string as an object of <c>&#39;a</ c>.
/// </summary>
/// <param name="yamlStr">Yaml string to load</param>
/// <exception cref="FsYaml.FsYamlException">If loading fails</exception>
let load<'a> yamlStr = Representation.parse yamlStr |> Native.construct<'a> TypeDefinitions.defaultDefinitions

/// <summary>
/// Loads a Yaml string as an <c>&#39;a</ c> object based on the specified type information and the default type information.
/// The specified type information takes precedence over the default type information. You can then override the load behavior by specifying the same type as the default type information.
/// </summary>
/// <param name="customDefinitions">User-defined type information</param>
/// <param name="yamlStr">Yaml string to load</param>
/// <exception cref="FsYaml.FsYamlException">If loading fails</exception>
let loadWith<'a> customDefinitions yamlStr = Representation.parse yamlStr |> Native.construct<'a> (Seq.append customDefinitions TypeDefinitions.defaultDefinitions)

/// <summary>
/// Load the Yaml string as an object of <c>&#39;a</ c>. Returns None if loading fails.
/// </summary>
/// <param name="yamlStr">Yaml string to load</param>
let tryLoad<'a> yamlStr =
  try
    Some (load<'a> yamlStr)
  with
    _ -> None

/// <summary>
/// Loads a Yaml string as an <c>&#39;a</ c> object based on the specified type information and the default type information. Returns None if loading fails.
/// The specified type information takes precedence over the default type information. You can then override the load behavior by specifying the same type as the default type information.
/// </summary>
/// <param name="customDefinitions">User-defined type information</param>
/// <param name="yamlStr">Yaml string to load</param>
let tryLoadWith<'a> customDefinitions yamlStr =
  try
    Some (loadWith<'a> customDefinitions yamlStr)
  with
    _ -> None

/// <summary>
/// Load the Yaml string as an object of <c>typ</ c>.
/// </summary>
/// <param name="typ">Type to load</param>
/// <param name="yamlStr">Yaml string to load</param>
/// <exception cref="FsYaml.FsYamlException">If loading fails</exception>
let loadUntyped typ yamlStr = Representation.parse yamlStr |> Native.constructUntyped typ TypeDefinitions.defaultDefinitions

/// <summary>
/// Loads a Yaml string as an <c>typ</c> object based on the specified type information and the default type information.
/// The specified type information takes precedence over the default type information. You can then override the load behavior by specifying the same type as the default type information.
/// </summary>
/// <param name="typ">type to load</param>
/// <param name="customDefinitions">User-defined type information</param>
/// <param name="yamlStr">Yaml string to load</param>
let loadWithUntyped typ customDefinitions yamlStr = Representation.parse yamlStr |> Native.constructUntyped typ (Seq.append customDefinitions TypeDefinitions.defaultDefinitions)

/// <summary>
/// Load the Yaml string as an object of <c>typ</c>. Returns None if loading fails.
/// </summary>
/// <param name="typ">type to load</param>
/// <param name="yamlStr">Yaml string to load</param>
let tryLoadUntyped typ yamlStr =
  try
    Some (loadUntyped typ yamlStr)
  with
    _ -> None

/// <summary>
/// Loads a Yaml string as an <c>typ</c> object based on the specified type information and the default type information. Returns None if loading fails.
/// The specified type information takes precedence over the default type information. You can then override the load behavior by specifying the same type as the default type information.
/// </summary>
/// <param name="typ">type to load</param>
/// <param name="customDefinitions">User-defined type information</param>
/// <param name="yamlStr">Yaml string to load</param>
let tryLoadWithUntyped typ customDefinitions yamlStr =
  try
    Some (loadWithUntyped typ customDefinitions yamlStr)
  with
    _ -> None

/// <summary>
/// Dump the object to a Yaml string.
/// </summary>
/// <param name="obj">Object to dump</param>
/// <exception cref="FsYaml.FsYamlException">If the dump fails</exception>
let dump<'a> obj = Native.represent<'a> TypeDefinitions.defaultDefinitions obj |> Representation.present


/// <summary>
/// Dump the object to a Yaml string based on the specified type information and the default type information.
/// The specified type information takes precedence over the default type information. You can then override the dump behavior by specifying the same type as the default type information.
/// </summary>
/// <param name="customDefinitions">User-defined type information</param>
/// <param name="obj">Objects to dump</param>
/// <exception cref="FsYaml.FsYamlException">If dump fails</exception>
let dumpWith<'a> customDefinitions obj = Native.represent<'a> (Seq.append customDefinitions TypeDefinitions.defaultDefinitions) obj |> Representation.present
