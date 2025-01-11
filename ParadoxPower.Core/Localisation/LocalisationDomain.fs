namespace ParadoxPower.Localisation

open ParadoxPower.Utilities.Position

[<Struct>]
type Entry =
    { key: string
      value: char option
      desc: string
      position: Range
      errorRange: Range option }
