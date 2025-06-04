using System;
using System.ComponentModel;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Microsoft.FSharp.Collections;
using ParadoxPower.Parser;
using ParadoxPower.Process;
using ParadoxPower.Utilities;
#if RELEASE
using System.Runtime.CompilerServices;
#endif

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

    /// <summary>
    /// 尝试在当前 <see cref="Node"/> 中查找指定键的 <see cref="Leaf"/>
    /// </summary>
    /// <param name="node"></param>
    /// <param name="key">查找的键, 大小写不敏感</param>
    /// <param name="leaf"></param>
    /// <returns>找到值时返回<c>true</c>, 反之为<c>false</c></returns>
    public static bool TryGetLeaf(this Node node, string key, [NotNullWhen(true)] out Leaf? leaf)
    {
        foreach (var leafValue in node.Leaves)
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

    /// <summary>
    ///
    /// </summary>
    /// <param name="n"></param>
    /// <param name="key">查找的键, 大小写不敏感</param>
    /// <param name="node"></param>
    /// <returns>找到值时返回<c>true</c>, 反之为<c>false</c></returns>
    public static bool TryGetNode(this Node n, string key, [NotNullWhen(true)] out Node? node)
    {
        foreach (var child in n.AllArray)
        {
            if (
                child.TryGetNode(out var childNode)
                && childNode.Key.Equals(key, StringComparison.OrdinalIgnoreCase)
            )
            {
                node = childNode;
                return true;
            }
        }

        node = null;
        return false;
    }

    /// <summary>
    /// 按顺序查找指定键的嵌套 <see cref="Node"/>
    /// </summary>
    /// <param name="n"></param>
    /// <param name="node"></param>
    /// <param name="keys">查找的键, 大小写不敏感</param>
    /// <returns>找到值时返回<c>true</c>, 反之为<c>false</c></returns>
    public static bool TryGetNode(
        this Node n,
        [NotNullWhen(true)] out Node? node,
        params ReadOnlySpan<string> keys
    )
    {
        var currentNode = n;
        foreach (string key in keys)
        {
            if (!currentNode.TryGetNode(key, out currentNode))
            {
                node = null;
                return false;
            }
        }

        node = currentNode;
        return true;
    }

    /// <summary>
    /// 尝试获取 <see cref="Child"/> 中的 <see cref="Leaf"/>
    /// </summary>
    /// <param name="child"></param>
    /// <param name="leaf"></param>
    /// <returns>当 <see cref="Child"/> 为 <see cref="Leaf"/> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetLeaf(this Child child, [NotNullWhen(true)] out Leaf? leaf)
    {
        if (child.IsLeafChild)
        {
#if DEBUG
            leaf = ((Child.LeafChild)child).Item;
#else
            leaf = Unsafe.As<Child.LeafChild>(child).Item;
#endif
            return true;
        }

        leaf = null;
        return false;
    }

    /// <summary>
    /// 尝试获取 <see cref="Child"/> 中的 <see cref="Node"/>
    /// </summary>
    /// <param name="child"></param>
    /// <param name="node"></param>
    /// <returns>当 <see cref="Child"/> 为 <see cref="Node"/> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetNode(this Child child, [NotNullWhen(true)] out Node? node)
    {
        if (child.IsNodeChild)
        {
#if DEBUG
            node = ((Child.NodeChild)child).Item;
#else
            node = Unsafe.As<Child.NodeChild>(child).Item;
#endif
            return true;
        }

        node = null;
        return false;
    }

    /// <summary>
    /// 尝试获取 <see cref="Child"/> 中的 <see cref="LeafValue"/>
    /// </summary>
    /// <param name="child"></param>
    /// <param name="leafValue"></param>
    /// <returns>当 <see cref="Child"/> 为 <see cref="LeafValue"/> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetLeafValue(
        this Child child,
        [NotNullWhen(true)] out LeafValue? leafValue
    )
    {
        if (child.IsLeafValueChild)
        {
#if DEBUG
            leafValue = ((Child.LeafValueChild)child).Item;
#else
            leafValue = Unsafe.As<Child.LeafValueChild>(child).Item;
#endif
            return true;
        }

        leafValue = null;
        return false;
    }

    /// <summary>
    /// 尝试获取 <see cref="Child"/> 中的 <see cref="Comment"/>
    /// </summary>
    /// <param name="child"></param>
    /// <param name="comment"></param>
    /// <returns>当 <see cref="Child"/> 为 <see cref="Comment"/> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetComment(this Child child, [NotNullWhen(true)] out Comment? comment)
    {
        if (child.IsCommentChild)
        {
#if DEBUG
            comment = ((Child.CommentChild)child).Item;
#else
            comment = Unsafe.As<Child.CommentChild>(child).Item;
#endif
            return true;
        }

        comment = null;
        return false;
    }

    /// <summary>
    /// 尝试获取 <see cref="Child"/> 中的 <see cref="ValueClause"/>
    /// </summary>
    /// <param name="child"></param>
    /// <param name="valueClause"></param>
    /// <returns>当 <see cref="Child"/> 为 <see cref="ValueClause"/> 时返回<c>true</c>, 反之返回<c>false</c></returns>
    public static bool TryGetValueClause(
        this Child child,
        [NotNullWhen(true)] out ValueClause? valueClause
    )
    {
        if (child.IsValueClauseChild)
        {
#if DEBUG
            valueClause = ((Child.ValueClauseChild)child).Item;
#else
            valueClause = Unsafe.As<Child.ValueClauseChild>(child).Item;
#endif
            return true;
        }

        valueClause = null;
        return false;
    }

    /// <summary>
    /// 将节点转化为脚本字符串, 应该在根节点上调用
    /// </summary>
    /// <param name="rootNode"></param>
    /// <returns></returns>
    public static string ToScript(this Node rootNode)
    {
        return Printer.PrettyPrintStatements(
            rootNode.AllArray.Select(child => GetRawStatement(child, string.Empty))
        );
    }

    private static Types.Statement GetRawStatement(Child child, string key)
    {
        if (child.TryGetNode(out var node))
        {
            return node.ToRaw;
        }

        if (child.TryGetLeaf(out var leaf))
        {
            return leaf.ToRaw;
        }

        if (child.TryGetLeafValue(out var value))
        {
            return value.ToRaw;
        }

        if (child.TryGetComment(out var comment))
        {
            return Types.Statement.NewCommentStatement(comment);
        }

        if (child.TryGetValueClause(out var clause))
        {
            var keys = new Types.Statement[clause.Keys.Length + 1];
            for (int i = 0; i < keys.Length; i++)
            {
                keys[i] = Types.Statement.NewValue(
                    Position.Range.Zero,
                    Types.Value.NewString(clause.Keys[i])
                );
            }
            keys[^1] = Types.Statement.NewValue(
                clause.Position,
                Types.Value.NewClause(clause.ToRaw)
            );

            return Types.Statement.NewKeyValue(
                Types.PosKeyValue.NewPosKeyValue(
                    clause.Position,
                    Types.KeyValueItem.NewKeyValueItem(
                        Types.Key.NewKey(key),
                        Types.Value.NewClause(ListModule.OfArray(keys)),
                        Types.Operator.Equals
                    )
                )
            );
        }

        throw new InvalidEnumArgumentException(nameof(child));
    }
}
