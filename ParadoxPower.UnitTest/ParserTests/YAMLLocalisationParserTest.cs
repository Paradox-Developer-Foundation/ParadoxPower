using ParadoxPower.CSharp;
using ParadoxPower.Localisation;

namespace ParadoxPower.UnitTest.ParserTests;

[TestFixture(TestOf = typeof(YAMLLocalisationParser))]
public sealed class YAMLLocalisationParserTest
{
    private const string Text = """
        #comment
        l_simp_chinese:#comment
        #comment
        key1: " value1" #comment1
        key2:2 "value2"
        key3: "" #comment2
        key4: "va\"lue4" #comment3
        key5:5 "va\"lue5" #comment4
        key6:"value6"#comment4
        #comment5
        """;

    private const string Failure = """
        l_simp_chinese:
         key1:
        """;

    [Test]
    public void ParseTest()
    {
        var result = YAMLLocalisationParser.ParseLocText(Text, "");
        var failure = YAMLLocalisationParser.ParseLocText(Failure, "");

        Assert.That(result.IsSuccess, Is.True);
        Assert.That(result.IsFailure, Is.False);
        Assert.That(failure.IsSuccess, Is.False);
        Assert.That(failure.IsFailure, Is.True);
    }

    [Test]
    public void ParseResultTest()
    {
        var result = YAMLLocalisationParser.ParseLocText(Text, "").GetResult();

        var key1 = result.Entries.First(loc => loc.Key == "key1");
        var key2 = result.Entries.First(loc => loc.Key == "key2");
        var key3 = result.Entries.First(loc => loc.Key == "key3");
        var key4 = result.Entries.First(loc => loc.Key == "key4");
        var key5 = result.Entries.First(loc => loc.Key == "key5");
        var key6 = result.Entries.First(loc => loc.Key == "key6");

        Assert.That(key1.Desc, Is.EqualTo(" value1"));
        Assert.That(key1.Value, Is.Null);
        Assert.That(key2.Desc, Is.EqualTo("value2"));
        Assert.That(key2.Value, Is.EqualTo('2'));
        Assert.That(key3.Desc, Is.EqualTo(""));
        Assert.That(key4.Desc, Is.EqualTo("va\"lue4"));
        Assert.That(key4.Value, Is.Null);
        Assert.That(key5.Desc, Is.EqualTo("va\"lue5"));
        Assert.That(key5.Value, Is.EqualTo('5'));
        Assert.That(key6.Desc, Is.EqualTo("value6"));
        Assert.That(key1.Position, Is.EqualTo(result.Entries.First().Position));
    }

    [Test]
    public void ParseKeyTest()
    {
        var result = YAMLLocalisationParser.ParseLocText(Text, "test.txt").GetResult();

        Assert.That(result.Key, Is.EqualTo("l_simp_chinese"));
    }
}
