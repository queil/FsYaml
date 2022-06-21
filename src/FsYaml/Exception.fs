namespace FsYaml

open System
open FsYaml.RepresentationTypes

/// <summary>
/// Exception thrown when <c>load</c> or <c>dump</c> fails.
/// </summary>
type FsYamlException(msg, ex: exn) =
  inherit Exception(msg, ex)

  new (msg) = FsYamlException(msg, null)

  /// <summary>
  /// Create an instance of this class.
  /// </summary>
  /// <param name="innerException"> Caused exception </param>
  /// <param name="format">Composite format specification string</param>
  /// <param name="args">Formatting object</param>
  static member Create(innerException, format, [<ParamArray>] args: obj[]) =
    let msg = String.Format(format, args)
    FsYamlException(msg, innerException)

  /// <summary>
  /// Create an instance of this class.
  /// </summary>
  /// <param name="format">Composite format specification string</param>
  /// <param name="args">Formatting object</param>
  static member Create(format, [<ParamArray>] args: obj[]) = FsYamlException.Create(null, format, args)

  /// <summary>
  /// Create an instance of this class.
  /// </summary>
  /// <param name="innerException">Caused exception</ param>
  /// <param name="position">The position in Yaml where this exception occurs</ param>
  /// <param name="format">Composite format specification string</ param>
  /// <param name="args">Formatting object</ param>
  static member WithPosition(innerException, position, format, [<ParamArray>] args: obj[]) =
    let msg = String.Format(format, args)
    let msg =
      match position with
      | Some p -> $"%s{msg} (Line=%d{p.Line}, Column=%d{p.Column})"
      | None -> msg
    FsYamlException(msg, innerException)

  /// <summary>
  /// Create an instance of this class.
  /// </summary>
  /// <param name="position">The position in Yaml where this exception occurs</param>
  /// <param name="format">Composite format specification string</param>
  /// <param name="args">Formatting object</param>
  static member WithPosition(position, format, [<ParamArray>] args: obj[]) = FsYamlException.WithPosition(null, position, format, args)

  /// <summary>
  /// Create an instance of this class.
  /// </summary>
  /// <param name="innerException">Caused exception</param>
  /// <param name="yaml">Yaml that caused this exception</param>
  /// <param name="format">Composite format specification string</param>
  /// <param name="args">Formatting object</param>
  static member WithYaml(innerException, yaml, format, [<ParamArray>] args: obj[]) = FsYamlException.WithPosition(innerException, YamlObject.position yaml, format, args)

  /// <summary>
  /// Create an instance of this class.
  /// </summary>
  /// <param name="yaml">Yaml that caused this exception</param>
  /// <param name="format">Composite format specification string</param>
  /// <param name="args">Formatting object</ param>
  static member WithYaml(yaml, format, [<ParamArray>] args: obj[]) = FsYamlException.WithYaml(null, yaml, format, args)
