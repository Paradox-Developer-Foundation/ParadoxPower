using ParadoxPower.Parser;
using ParadoxPower.Utilities;
using Shouldly;

namespace ParadoxPower.UnitTest.ProcessTests;

[TestFixture(TestOf = typeof(Types.Position))]
public sealed class RangeTest
{
    // 不要修改这个文本, 否则测试会失败
    private const string Text = "node = { leaf = 1 values = {1 2 3  } }";

    [Test]
    public void NodeEndTest()
    {
        var root = ParserHelper.Parse(Text);
        var node = root.GetChild("node").Value;

        node.Position.Start.Column.ShouldBe(0);
        node.Position.End.Column.ShouldBe(Text.Length);

        node.Position.Start.Line.ShouldBe(1);
        node.Position.End.Line.ShouldBe(1);
    }

    [Test]
    public void LeafEndTest()
    {
        var root = ParserHelper.Parse(Text);
        var node = root.GetChild("node").Value;
        var leaf = node.GetLeaf("leaf").Value;

        leaf.Position.Start.Column.ShouldBe(9);
        leaf.Position.End.Column.ShouldBe(17);

        leaf.Position.Start.Line.ShouldBe(1);
        leaf.Position.End.Line.ShouldBe(1);

        GetRangeLength(leaf.Position).ShouldBe(8);
    }

    [Test]
    public void ValuesEndTest()
    {
        var root = ParserHelper.Parse(Text);
        var values = root.GetChild("node").Value.GetChild("values").Value;

        values.Position.Start.Column.ShouldBe(18);
        values.Position.End.Column.ShouldBe(36);

        values.Position.Start.Line.ShouldBe(1);
        values.Position.End.Line.ShouldBe(1);

        foreach (var leafValue in values.LeafValues)
        {
            GetRangeLength(leafValue.Position).ShouldBe(1);
        }
    }

    private static int GetRangeLength(Position.Range position)
    {
        position.Start.Line.ShouldBe(position.End.Line);

        return position.End.Column - position.Start.Column;
    }
}
