using ParadoxPower.CSharpExtensions;
using ParadoxPower.Parser;
using ParadoxPower.Process;
using Shouldly;

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
            node1.ShouldNotBeNull();
            node2.ShouldNotBeNull();
            node3.ShouldBeNull();
        });

        Assert.Multiple(() =>
        {
            node1?.Key.ShouldBe("node1");
            node2?.Key.ShouldBe("node2");
        });
    }

    [Test]
    public void TryGetBoolValueTest()
    {
        var value = Types.Value.NewBool(false);

        value.TryGetBool(out bool boolValue).ShouldBeTrue();
        boolValue.ShouldBeFalse();
        value.TryGetInt(out _).ShouldBeFalse();
        value.TryGetClause(out _).ShouldBeFalse();
        value.TryGetDecimal(out _).ShouldBeFalse();
        value.TryGetString(out _).ShouldBeFalse();
        value.TryGetQString(out _).ShouldBeFalse();
    }

    [Test]
    public void TryGetIntValueTest()
    {
        var value = Types.Value.NewInt(11);

        value.TryGetInt(out int intValue).ShouldBeTrue();
        intValue.ShouldBe(11);
        value.TryGetBool(out _).ShouldBeFalse();
        value.TryGetClause(out _).ShouldBeFalse();
        value.TryGetDecimal(out _).ShouldBeFalse();
        value.TryGetString(out _).ShouldBeFalse();
        value.TryGetQString(out _).ShouldBeFalse();
    }

    [Test]
    public void TryGetDecimalValueTest()
    {
        var value = Types.Value.NewFloat(11.11m);

        value.TryGetDecimal(out decimal decimalValue).ShouldBeTrue();
        decimalValue.ShouldBe(11.11m);
        value.TryGetBool(out _).ShouldBeFalse();
        value.TryGetInt(out _).ShouldBeFalse();
        value.TryGetClause(out _).ShouldBeFalse();
        value.TryGetString(out _).ShouldBeFalse();
        value.TryGetQString(out _).ShouldBeFalse();
    }

    [Test]
    public void TryGetStringValueTest()
    {
        var value = Types.Value.NewStringValue("test");

        value.TryGetString(out string? stringValue).ShouldBeTrue();
        stringValue.ShouldBe("test");
        value.TryGetBool(out _).ShouldBeFalse();
        value.TryGetInt(out _).ShouldBeFalse();
        value.TryGetClause(out _).ShouldBeFalse();
        value.TryGetDecimal(out _).ShouldBeFalse();
        value.TryGetQString(out _).ShouldBeFalse();
    }

    [Test]
    public void TryGetQStringValueTest()
    {
        var value = Types.Value.NewQStringValue("test");

        value.TryGetQString(out string? qStringValue).ShouldBeTrue();
        qStringValue.ShouldBe("test");
        value.TryGetBool(out _).ShouldBeFalse();
        value.TryGetInt(out _).ShouldBeFalse();
        value.TryGetClause(out _).ShouldBeFalse();
        value.TryGetDecimal(out _).ShouldBeFalse();
        value.TryGetString(out _).ShouldBeFalse();
    }
}
