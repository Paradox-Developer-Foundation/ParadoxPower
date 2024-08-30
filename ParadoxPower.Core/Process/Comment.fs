namespace ParadoxPower.Process

open ParadoxPower.Utilities.Position

[<NoEquality; NoComparison>]
type Comment =
    {
        Position: Range
        Comment: string
    }