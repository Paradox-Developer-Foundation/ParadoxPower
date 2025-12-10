namespace ParadoxPower.Process

open ParadoxPower.Utilities.Position

//TODO: 补上隐式转换
[<NoEquality; NoComparison>]
type Comment =
    {
        Position: Range
        Comment: string
    }