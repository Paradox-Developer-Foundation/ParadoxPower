using System;
using System.Diagnostics.CodeAnalysis;
using ParadoxPower.Parser;
using ParadoxPower.Process;

namespace ParadoxPower.CSharpExtensions;

public static class Extensions
{
    public static void AddLeaf(
        this Node node,
        string key,
        int value,
        Types.Operator op = Types.Operator.Equals
    )
    {
        node.AddChild(new Leaf(key, Types.Value.NewInt(value), op));
    }

    public static void AddLeaf(
        this Node node,
        string key,
        decimal value,
        Types.Operator op = Types.Operator.Equals
    )
    {
        node.AddChild(new Leaf(key, Types.Value.NewFloat(value), op));
    }

    public static void AddLeaf(
        this Node node,
        string key,
        bool value,
        Types.Operator op = Types.Operator.Equals
    )
    {
        node.AddChild(new Leaf(key, Types.Value.NewBool(value), op));
    }

    public static void AddLeafString(
        this Node node,
        string key,
        string value,
        Types.Operator op = Types.Operator.Equals
    )
    {
        node.AddChild(new Leaf(key, Types.Value.NewStringValue(value), op));
    }

    public static void AddLeafQString(
        this Node node,
        string key,
        string value,
        Types.Operator op = Types.Operator.Equals
    )
    {
        node.AddChild(new Leaf(key, Types.Value.NewQStringValue(value), op));
    }

    public static bool TryGetLeaf(this Node node, string key, [NotNullWhen(true)] out Leaf? leaf)
    {
        foreach (var leafValue in node.GetLeavesArray())
        {
            if (StringComparer.OrdinalIgnoreCase.Equals(leafValue.Key, key))
            {
                leaf = leafValue;
                return true;
            }
        }

        leaf = null;
        return false;
    }

    public static bool TryGetNode(this Node n, string key, [NotNullWhen(true)] out Node? node)
    {
        foreach (var child in n.AllArray)
        {
            if (!child.IsNodeChild)
            {
                continue;
            }

            var childNode = child.node;
            if (StringComparer.OrdinalIgnoreCase.Equals(childNode.Key, key))
            {
                node = childNode;
                return true;
            }
        }

        node = null;
        return false;
    }
}
