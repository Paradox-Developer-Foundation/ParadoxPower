using ParadoxPower.CSharpExtensions;
using ParadoxPower.Process;

namespace ParadoxPower.UnitTest.Extensions;

[TestFixture]
public class ExtensionsTest
{
    private const string Text = """
        node1 = {
            node2 = {
                key = value
            }
        }
        """;

    [Test]
    public void TryGetNodeTest()
    {
        var node = ParserHelper.Parse(Text);

        Node? node1 = null;
        Node? node2 = null;
        Node? node3 = null;

        Assert.Multiple(() =>
        {
            Assert.That(node.TryGetNode(out node1, ["node1"]), Is.True);
            Assert.That(node.TryGetNode(out _, "node1"), Is.True);
            Assert.That(node.TryGetNode(out node2, ["node1", "node2"]), Is.True);
            Assert.That(node.TryGetNode(out node3, ["node1", "node2", "node3"]), Is.False);
            Assert.That(node.TryGetNode(out _, ["node1", "node2", "node3", "key"]), Is.False);
        });

        Assert.Multiple(() =>
        {
            Assert.That(node1, Is.Not.Null);
            Assert.That(node2, Is.Not.Null);
            Assert.That(node3, Is.Null);
        });
        
        Assert.Multiple(() =>
        {
            Assert.That(node1.Key, Is.EqualTo("node1"));
            Assert.That(node2.Key, Is.EqualTo("node2"));
        });
    }
}
