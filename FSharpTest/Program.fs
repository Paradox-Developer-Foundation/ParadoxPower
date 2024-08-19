module Program

open ParadoxPower.Parser
open ParadoxPower.Process
open ParadoxPower.Utilities.Position

let node = Node("")
let str = "Hello, world!"
let leaf = Child.LeafC(Leaf("local_supplies", Value.Int(3), Range.Zero, Operator.Equals))
node.SetValue str leaf