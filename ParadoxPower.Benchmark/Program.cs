using System.Buffers;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using FParsec;
using Microsoft.FSharp.Core;
using ParadoxPower.CSharp;
using ParadoxPower.Localisation;
using ParadoxPower.Parser;
using ParadoxPower.Process;
using ParadoxPower.Utilities;

namespace ParadoxPower.Benchmark;

[MemoryDiagnoser]
public class Program
{
    private const string Text = """

        state={
        	id=1
        	name="STATE_1" # Corsica
        	manpower = 322900
        	
        	state_category = town

        	history={
        		owner = FRA
        		victory_points = { 3838 1 }
        		buildings = {
        			infrastructure = 2
        			industrial_complex = 1
        			air_base = 1
        			3838 = {
        				naval_base = 3
        			}
        		}
        		add_core_of = COR
        		add_core_of = FRA
        	}

        	provinces={
        		3838 9851 11804 
        	}

        	local_supplies=0.0 
        }

        """;

    private Node _node;

    // private readonly FSharpFunc<char, bool> isAnyOf = ;

    [GlobalSetup]
    public void Setup()
    {
        _node = Parsers.ProcessStatements("123", "123",
            Parsers.ParseScriptFile("123.txt", Text).GetResult());
    }

    public static void Main(string[] args)
    {
        BenchmarkRunner.Run<Program>();
    }

    private const string Comment = "# 12312312312";
    private readonly CharStream<Unit> _stream = new(Comment, 0, Comment.Length);

    [Benchmark(Baseline = true)]
    public object Raw()
    {
		return _node.ToRaw;
    }
}
