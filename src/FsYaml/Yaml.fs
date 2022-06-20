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
/// Yaml文字列を<c>'a</c>のオブジェクトとしてロードします。
/// </summary>
/// <param name="yamlStr">ロードするYaml文字列</param>
/// <exception cref="FsYaml.FsYamlException">ロードに失敗した場合</exception>
let load<'a> yamlStr = Representation.parse yamlStr |> Native.construct<'a> TypeDefinitions.defaultDefinitions

/// <summary>
/// 指定した型情報とデフォルトの型情報をもとに、Yaml文字列を<c>'a</c>のオブジェクトとしてロードします。
/// 指定された型情報は、デフォルトの型情報より優先されます。そして、デフォルトの型情報と同じ型を指定すると、ロードの挙動を上書きできます。
/// </summary>
/// <param name="customDefinitions">ユーザが定義した型情報</param>
/// <param name="yamlStr">ロードするYaml文字列</param>
/// <exception cref="FsYaml.FsYamlException">ロードに失敗した場合</exception>
let loadWith<'a> customDefinitions yamlStr = Representation.parse yamlStr |> Native.construct<'a> (Seq.append customDefinitions TypeDefinitions.defaultDefinitions)

/// <summary>
/// Yaml文字列を<c>'a</c>のオブジェクトとしてロードします。ロードに失敗した場合はNoneを返します。
/// </summary>
/// <param name="yamlStr">ロードするYaml文字列</param>
let tryLoad<'a> yamlStr =
  try
    Some (load<'a> yamlStr)
  with
    _ -> None

/// <summary>
/// 指定した型情報とデフォルトの型情報をもとに、Yaml文字列を<c>'a</c>のオブジェクトとしてロードします。ロードに失敗した場合はNoneを返します。
/// 指定された型情報は、デフォルトの型情報より優先されます。そして、デフォルトの型情報と同じ型を指定すると、ロードの挙動を上書きできます。
/// </summary>
/// <param name="customDefinitions">ユーザが定義した型情報</param>
/// <param name="yamlStr">ロードするYaml文字列</param>
let tryLoadWith<'a> customDefinitions yamlStr =
  try
    Some (loadWith<'a> customDefinitions yamlStr)
  with
    _ -> None

/// <summary>
/// Yaml文字列を<c>typ</c>のオブジェクトとしてロードします。
/// </summary>
/// <param name="typ">ロードする型</param>
/// <param name="yamlStr">ロードするYaml文字列</param>
/// <exception cref="FsYaml.FsYamlException">ロードに失敗した場合</exception>
let loadUntyped typ yamlStr = Representation.parse yamlStr |> Native.constructUntyped typ TypeDefinitions.defaultDefinitions

/// <summary>
/// 指定した型情報とデフォルトの型情報をもとに、Yaml文字列を<c>typ</c>のオブジェクトとしてロードします。
/// 指定された型情報は、デフォルトの型情報より優先されます。そして、デフォルトの型情報と同じ型を指定すると、ロードの挙動を上書きできます。
/// </summary>
/// <param name="typ">ロードする型</param>
/// <param name="customDefinitions">ユーザが定義した型情報</param>
/// <param name="yamlStr">ロードするYaml文字列</param>
let loadWithUntyped typ customDefinitions yamlStr = Representation.parse yamlStr |> Native.constructUntyped typ (Seq.append customDefinitions TypeDefinitions.defaultDefinitions)

/// <summary>
/// Yaml文字列を<c>typ</c>のオブジェクトとしてロードします。ロードに失敗した場合はNoneを返します。
/// </summary>
/// <param name="typ">ロードする型</param>
/// <param name="yamlStr">ロードするYaml文字列</param>
let tryLoadUntyped typ yamlStr =
  try
    Some (loadUntyped typ yamlStr)
  with
    _ -> None

/// <summary>
/// 指定した型情報とデフォルトの型情報をもとに、Yaml文字列を<c>typ</c>のオブジェクトとしてロードします。ロードに失敗した場合はNoneを返します。
/// 指定された型情報は、デフォルトの型情報より優先されます。そして、デフォルトの型情報と同じ型を指定すると、ロードの挙動を上書きできます。
/// </summary>
/// <param name="typ">ロードする型</param>
/// <param name="customDefinitions">ユーザが定義した型情報</param>
/// <param name="yamlStr">ロードするYaml文字列</param>
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
/// 指定した型情報とデフォルトの型情報をもとに、オブジェクトをYaml文字列へダンプします。
/// 指定された型情報は、デフォルトの型情報より優先されます。そして、デフォルトの型情報と同じ型を指定すると、ダンプの挙動を上書きできます。
/// </summary>
/// <param name="customDefinitions">ユーザが定義した型情報</param>
/// <param name="obj">ダンプするオブジェクト</param>
/// <exception cref="FsYaml.FsYamlException">ダンプに失敗した場合</exception>
let dumpWith<'a> customDefinitions obj = Native.represent<'a> (Seq.append customDefinitions TypeDefinitions.defaultDefinitions) obj |> Representation.present