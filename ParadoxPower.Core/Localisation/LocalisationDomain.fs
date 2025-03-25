namespace ParadoxPower.Localisation

open ParadoxPower.Utilities.Position

[<Struct>]
#if NET5_0_OR_GREATER
[<System.Runtime.CompilerServices.IsReadOnly>]
#endif
type Entry =
    { key: string
      value: char option
      desc: string
      position: Range
      errorRange: Range option }
