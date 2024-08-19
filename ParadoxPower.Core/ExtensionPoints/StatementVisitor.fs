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
        | NodeC n -> this.Visit(n)
        | LeafC l -> this.Visit(l)
        | LeafValueC lv -> this.Visit(lv)
        | ValueClauseC vc -> this.Visit(vc)
        | CommentC(r, c) -> this.Visit((r, c))
