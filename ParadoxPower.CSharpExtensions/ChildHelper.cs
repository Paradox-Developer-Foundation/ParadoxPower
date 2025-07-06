using System.Collections.Generic;
using System.Linq;
using ParadoxPower.Parser;
using ParadoxPower.Process;

namespace ParadoxPower.CSharpExtensions;

public static class ChildHelper
{
    public static Child Leaf(string key, int value, Types.Operator op = Types.Operator.Equals)
    {
        return new Leaf(key, Types.Value.NewInt(value), op);
    }

    public static Child Leaf(string key, bool value, Types.Operator op = Types.Operator.Equals)
    {
        return new Leaf(key, Types.Value.NewBool(value), op);
    }

    public static Child Leaf(string key, decimal value, Types.Operator op = Types.Operator.Equals)
    {
        return new Leaf(key, Types.Value.NewFloat(value), op);
    }

    public static Child LeafString(
        string key,
        string value,
        Types.Operator op = Types.Operator.Equals
    )
    {
        return new Leaf(key, Types.Value.NewString(value), op);
    }

    public static Child LeafQString(
        string key,
        string value,
        Types.Operator op = Types.Operator.Equals
    )
    {
        return new Leaf(key, Types.Value.NewQString(value), op);
    }

    public static Child Node(string key)
    {
        return new Node(key);
    }

    public static Child Node(string key, IEnumerable<Child> children)
    {
        return Node(key, children.ToArray());
    }

    public static Child Node(string key, Child[] children)
    {
        var node = new Node(key) { AllArray = children };
        return node;
    }

    public static Child LeafValue(string value)
    {
        return Process.LeafValue.Create(Types.Value.NewString(value));
    }

    public static Child LeafValue(int value)
    {
        return Process.LeafValue.Create(Types.Value.NewInt(value));
    }
}
