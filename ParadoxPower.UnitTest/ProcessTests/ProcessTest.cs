using ParadoxPower.CSharp;
using ParadoxPower.Process;

namespace ParadoxPower.UnitTest.ProcessTests;

[TestFixture(TestOf = typeof(Node))]
public class ProcessTest
{
    private const string Text = """
                                key1 = value1
                                node1 = {
                                    key2 = value2
                                }
                                """;
    
    private Node _root;

    [SetUp]
    public void Setup()
    {
        _root = Parsers.ProcessStatements("123", "123",
            Parsers.ParseScriptFile("123.txt", Text).GetResult());
    }

    [Test]
    public void SetValueTest()
    {
        Assert.That(_root.Leaf("key1").Value.Value.ToRawString(), Is.EqualTo("value1"));
        
        const string newValue = "new value";
        _root.SetLeafValue("key1", newValue);
        
        Assert.That(_root.Leaf("key1").Value.Value.ToRawString(), Is.EqualTo(newValue));
    }
    
    [Test]
    public void LeavesTest()
    {
        Assert.That(_root.Leaves.Count(), Is.EqualTo(1));
        Assert.That(_root.LeavesOnlyRead.Count, Is.EqualTo(1));
    }
}