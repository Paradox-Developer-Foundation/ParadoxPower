namespace ParadoxPower.ExtensionPoints

open ParadoxPower.Process
open ParadoxPower.Utilities.Position

[<AbstractClass>]
type StatementVisitor() =
    abstract member Visit: Node -> Unit
    abstract member Visit: Leaf -> Unit
    abstract member Visit: LeafValue -> Unit
    abstract member Visit: ValueClause -> Unit
    abstract member Visit: (Range * string) -> Unit

    member this.Visit(x: Child) : Unit =
        match x with
        | NodeChild n -> this.Visit(n)
        | LeafChild l -> this.Visit(l)
        | LeafValueChild lv -> this.Visit(lv)
        | ValueClauseChild vc -> this.Visit(vc)
        | CommentChild(r, c) -> this.Visit((r, c))
