using ParadoxPower.CSharp;
using ParadoxPower.Process;

namespace ParadoxPower.UnitTest.ProcessTests;

[TestFixture(TestOf = typeof(Node))]
public class ProcessTest
{
    private const string Text = """
        # comment1
        key1 = value1
        node1 = {
            key2 = value2
        }
        """;

    private Node _root;

    [SetUp]
    public void Setup()
    {
        _root = Parsers.ProcessStatements(
            "123",
            "123",
            Parsers.ParseScriptFile("123.txt", Text).GetResult()
        );
    }

    [Test]
    public void SetValueTest()
    {
        // Assert.That(_root.TryGetLeaf("key1").Value.Value.ToRawString(), Is.EqualTo("value1"));
        //
        // const string newValue = "new value";
        // _root.SetLeafValue("key1", newValue);
        //
        // Assert.That(_root.TryGetLeaf("key1").Value.Value.ToRawString(), Is.EqualTo(newValue));
    }

    [Test]
    public void LeavesTest()
    {
        Assert.That(_root.Leaves.Count(), Is.EqualTo(1));
    }

    [Test]
    public void TryGetLeafTest()
    {
        Assert.That(_root.TryGetLeaf("key1", out var leaf), Is.True);
        Assert.That(leaf.Value.ToRawString(), Is.EqualTo("value1"));
        Assert.That(_root.TryGetLeaf("notKey", out var nullLeaf), Is.False);
        Assert.That(nullLeaf, Is.Null);
    }
    
    [Test]
    public void TryGetChildTest()
    {
        Assert.That(_root.TryGetChild("key1", out var nullNode), Is.False);
        Assert.That(nullNode, Is.Null);
        Assert.That(_root.TryGetChild("node1", out var node), Is.True);
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
}
