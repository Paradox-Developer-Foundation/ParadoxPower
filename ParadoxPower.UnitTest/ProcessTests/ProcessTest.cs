﻿using ParadoxPower.CSharp;
using ParadoxPower.CSharpExtensions;
using ParadoxPower.Parser;
using ParadoxPower.Process;
using ParadoxPower.Utilities;

namespace ParadoxPower.UnitTest.ProcessTests;

[TestFixture(TestOf = typeof(Node))]
public class ProcessTest
{
    private const string Text = """
        # comment1
        key1 = value1
        key2 = "value2"
        node1 = {
            key2 = value2
        }
        """;

    private Node _root;

    [SetUp]
    public void Setup()
    {
        _root = ParserHelper.Parse(Text);
    }

    [Test]
    public void LeavesTest()
    {
        Assert.That(_root.Leaves.ToArray(), Has.Length.EqualTo(2));
    }

    [Test]
    public void TryGetLeafTest()
    {
        Assert.That(_root.TryGetLeaf("key1", out var leaf), Is.True);
        Assert.That(leaf!.Value.ToRawString(), Is.EqualTo("value1"));
        Assert.That(_root.TryGetLeaf("notKey", out var nullLeaf), Is.False);
        Assert.That(nullLeaf, Is.Null);
    }

    [Test]
    public void TryGetNodeTest()
    {
        Assert.That(_root.TryGetNode("key1", out var nullNode), Is.False);
        Assert.That(nullNode, Is.Null);
        Assert.That(_root.TryGetNode("node1", out var node), Is.True);
        Assert.That(node, Is.Not.Null);
        Assert.That(node.Key, Is.EqualTo("node1"));
    }

    [Test]
    public void CommentsTest()
    {
        var comments = _root.Comments.ToArray();
        Assert.That(comments, Has.Length.EqualTo(1));
        Assert.That(comments[0].Comment, Is.EqualTo(" comment1"));
    }

    [Test]
    public void LeafParseResultTest()
    {
        var key1 = _root.Leaves.First(leaf => leaf.Key == "key1");
        var key2 = _root.Leaves.First(leaf => leaf.Key == "key2");

        Assert.That(key1, Is.Not.Null);
        Assert.That(key1.Value.ToRawString(), Is.EqualTo("value1"));
        Assert.That(key1.Value.IsString, Is.True);
        Assert.That(key2, Is.Not.Null);
        Assert.That(key2.Value.ToRawString(), Is.EqualTo("value2"));
        Assert.That(key2.Value.IsQString, Is.True);
    }

    [Test]
    public void AddChildTest()
    {
        var node = ParserHelper.Parse(Text);
        node.AddChild(
            Leaf.Create(
                Types.KeyValueItem.NewKeyValueItem(
                    Types.Key.NewKey("addKey"),
                    Types.Value.NewInt(1),
                    Types.Operator.Equals
                ),
                Position.Range.Zero
            )
        );
        Assert.That(node.TryGetLeaf("addKey", out var leaf), Is.True);
        Assert.That(leaf!.Value.ToRawString(), Is.EqualTo("1"));
        Assert.That(leaf.Value.IsInt, Is.True);
    }

    [Test]
    public void LeafTest()
    {
        var leaf = new Leaf("key1", Types.Value.NewString("value"), Types.Operator.Equals);
        Assert.Multiple(() =>
        {
            Assert.That(leaf.Key, Is.EqualTo("key1"));
            Assert.That(leaf.Value.ToRawString(), Is.EqualTo("value"));
            Assert.That(leaf.Operator, Is.EqualTo(Types.Operator.Equals));
            Assert.That(leaf.Value.IsString, Is.True);
        });
    }

    [Test]
    public void LeafValuesTest()
    {
        var leaf = LeafValue.Create(Types.Value.NewString("value"));
        Assert.That(leaf.Value.ToRawString(), Is.EqualTo("value"));
        Assert.That(leaf.Key, Is.EqualTo(leaf.Value.ToRawString()));
        Assert.That(leaf.ValueText, Is.EqualTo(leaf.Value.ToRawString()));
    }

    [Test]
    public void NodeTest()
    {
        var node = new Node("key1", Position.Range.Zero);
        Assert.Multiple(() =>
        {
            Assert.That(node.Key, Is.EqualTo("key1"));
            Assert.That(node.AllArray, Is.Empty);
            Assert.That(node.Leaves, Is.Empty);
            Assert.That(node.Comments, Is.Empty);
            Assert.That(node.Position, Is.EqualTo(Position.Range.Zero));
            Assert.That(node.Parent, Is.Null);
        });
    }

    [Test]
    public void ChildrenOrderTest()
    {
        var children = _root.AllArray;

        Assert.That(children[0].TryGetComment(out var common), Is.True);
        Assert.That(children[1].TryGetLeaf(out var leaf1), Is.True);
        Assert.That(children[2].TryGetLeaf(out var leaf2), Is.True);
        Assert.That(children[3].TryGetNode(out var node), Is.True);
        Assert.That(common, Is.Not.Null);
        Assert.That(leaf1, Is.Not.Null);
        Assert.That(leaf2, Is.Not.Null);
        Assert.That(node, Is.Not.Null);

        Assert.Multiple(() =>
        {
            Assert.That(common.Comment, Is.EqualTo(" comment1"));
            Assert.That(leaf1.Key, Is.EqualTo("key1"));
            Assert.That(leaf2.Key, Is.EqualTo("key2"));
            Assert.That(node.Key, Is.EqualTo("node1"));
        });
    }

    [Test]
    public void ParentTest()
    {
        var children = _root.AllArray;

        children[1].TryGetLeaf(out var leaf);
        children[3].TryGetNode(out var node);

        Assert.Multiple(() =>
        {
            Assert.That(node!.Parent, Is.SameAs(_root));
            Assert.That(node.Parent.Parent, Is.Null);
            Assert.That(leaf!.Parent, Is.SameAs(_root));
            Assert.That(leaf.Parent.Parent, Is.Null);
        });
    }
}
