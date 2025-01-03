using ParadoxPower.CSharp;
using ParadoxPower.Process;

namespace ParadoxPower.UnitTest;

public static class ParserHelper
{
    private static readonly Bogus.DataSets.System SystemFaker = new();

    public static Node Parse(string input)
    {
        string fileName = SystemFaker.FileName();
        return Parsers.ProcessStatements(
            fileName,
            SystemFaker.FilePath(),
            Parsers.ParseScriptFile(fileName, input).GetResult()
        );
    }
}
