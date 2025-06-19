namespace ParadoxPower.Localisation

open System
open ParadoxPower.Utilities.Position

[<NoEquality;NoComparison>]
type Entry =
    { Key: string
      Value: Nullable<char>
      Desc: string
      Position: Range
      ErrorRange: Nullable<Range> }
