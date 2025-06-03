namespace ParadoxPower.Localisation

open System
open ParadoxPower.Utilities.Position

[<Struct>]
#if NET5_0_OR_GREATER
[<System.Runtime.CompilerServices.IsReadOnly>]
#endif
type Entry =
    { Key: string
      Value: Nullable<char>
      Desc: string
      Position: Range
      ErrorRange: Nullable<Range> }
